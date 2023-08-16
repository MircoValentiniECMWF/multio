/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "Statistics.h"

#include <algorithm>
#include <unordered_map>
#include <iomanip>


#include "TemporalStatistics.h"
#include "eckit/exception/Exceptions.h"
#include "multio/LibMultio.h"
#include "multio/message/Message.h"
#include "multio/util/ScopedTimer.h"

namespace multio::action {

namespace {
    std::map<std::string,eckit::LocalConfiguration> getMatcherConfiguration(const ComponentConfiguration& compConf){
        std::map<std::string,eckit::LocalConfiguration> cfg;
        if ( compConf.parsedConfig().has("synoptic-filters") ){
            for ( auto& subComp : compConf.subComponents("synoptic-filters") ){
                if ( subComp.parsedConfig().has("name") ){
                    std::string name_ = subComp.parsedConfig().getString("name");
                    cfg[name_] = subComp.parsedConfig();
                }
                else {
                    std::ostringstream os;
                    os << "type keyword expected" << std::endl;
                    throw eckit::UserError(os.str(), Here());
                };
            }
        }
        return cfg;
    }

    int64_t normalizeMemory( int64_t mem ){
        int64_t tmp=1;
        while( tmp < mem){
            tmp *= 1024;
        }
        return tmp/1024;
    };

    std::string memoryUnits( int64_t mem ){
        static const std::vector<std::string> units={ "[B]", "[KB]", "[MB]", "[GB]", "[TB]", "[PB]", "[EB]"};
        int64_t tmp=1024;
        int64_t cnt = 0;
        while( mem > tmp ){
            cnt++;
            tmp *= 1024;
        }
        return units[cnt];
    };

}


Statistics::Statistics(const ComponentConfiguration& compConf) :
    ChainedAction{compConf},
    cfg_{compConf},
    operations_{compConf.parsedConfig().getStringVector("operations")},
    periodUpdater_{make_period_updater(compConf.parsedConfig().getString("output-frequency"))},
    IOmanager_{StatisticsIOFactory::instance().build(cfg_.restartLib(), cfg_.restartPath(), cfg_.restartPrefix())},
    matcherCfg_{getMatcherConfiguration(compConf)},
    profiler_{},
    nExecuteImplCalls_{0},
    nFlushCalls_{0},
    memoryUsed_{0},
    profileTime_{0},
    profileNCalls_{0}{}


void Statistics::DumpRestart() {
    if (cfg_.writeRestart()) {
        IOmanager_->reset();
        IOmanager_->setSuffix(periodUpdater_->name());
        for (auto it = fieldStats_.begin(); it != fieldStats_.end(); it++) {
            LOG_DEBUG_LIB(LibMultio) << "Restart for field with key :: " << it->first << ", "
                                     << it->second->cwin().currPointInSteps() << std::endl;
            IOmanager_->setCurrStep(it->second->cwin().currPointInSteps());
            IOmanager_->setPrevStep(it->second->cwin().lastFlushInSteps());
            IOmanager_->setKey(it->first);
            it->second->dump(IOmanager_, cfg_);
            it->second->win().updateFlush();
        }
    }
}


void Statistics::PrintProfilingInfo() {
    std::array<int64_t,20> tmpTime{static_cast<int64_t>(0)};
    std::array<int64_t,20> tmpCalls{static_cast<int64_t>(0)};
    int64_t lastMem = 0;

    // Read timing directly from oprations
    for (auto ite = fieldStats_.begin(); ite != fieldStats_.end(); ite++) {
        LOG_DEBUG_LIB(LibMultio) << " + Collect prfiling info " << std::endl;
        for (auto it = ite->second->collection_begin(); it != ite->second->collection_end(); ++it) {
            LOG_DEBUG_LIB(LibMultio) << " + Print profiling info related to operations" << std::endl;
            for ( int i=0; i<6; i++ ){
                tmpTime[i]  += (*it)->getTotalTimeNsec( i );
                tmpCalls[i] += (*it)->getNumberOfCalls( i );
            }
            lastMem +=  (*it)->memory_in_bytes();
            (*it)->resetProfiler();
        }
    }

    // Read timing from execut impl
    for ( int i=0; i<10; i++ ){
        tmpTime[i+6]  = profiler_[i].getTotalTimeNsec();
        tmpCalls[i+6] = profiler_[i].getNumberOfCalls();
        profiler_[i].reset();
    }

    // Update total timing 
    for ( int i=0; i<20; i++ ){
        profileTime_[i]   += tmpTime[i];
        profileNCalls_[i] += tmpCalls[i];
        
    }


    std::cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++" << std::endl;
    std::cout << " + Number of ExecuteImpl calls     :: " << nExecuteImplCalls_  << std::endl;
    std::cout << " + Number of Flush calls           :: " << nFlushCalls_        << std::endl;
    std::cout << " + Total memory for the operations :: " << static_cast<double>(lastMem)/normalizeMemory( lastMem ) <<  " " << memoryUnits( lastMem )      << std::endl;
    std::cout << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -" << std::endl;

    std::cout << " + Operation Last Init Time                      :: " << std::setw(16) << static_cast<double>(tmpTime[0]/1000)/1000000                            << " [s],  Operation Last Number Of Calls to Init         :: " << std::setw(16) << tmpCalls[0]<< std::endl;
    std::cout << " + Operation Last UpdateData Time                :: " << std::setw(16) << static_cast<double>(tmpTime[1]/1000)/1000000                            << " [s],  Operation Last Number Of Calls to UpdateData   :: " << std::setw(16) << tmpCalls[1]<< std::endl;
    std::cout << " + Operation Last UpdateWindow Time              :: " << std::setw(16) << static_cast<double>(tmpTime[2]/1000)/1000000                            << " [s],  Operation Last Number Of Calls to UpdatWindow  :: " << std::setw(16) << tmpCalls[2]<< std::endl;
    std::cout << " + Operation Last Compute Time                   :: " << std::setw(16) << static_cast<double>(tmpTime[3]/1000)/1000000                            << " [s],  Operation Last Number Of Calls to Compute      :: " << std::setw(16) << tmpCalls[3]<< std::endl;
    std::cout << " + Operation Last Dump Time                      :: " << std::setw(16) << static_cast<double>(tmpTime[4]/1000)/1000000                            << " [s],  Operation Last Number Of Calls to Dump         :: " << std::setw(16) << tmpCalls[4]<< std::endl;
    std::cout << " + Operation Last Load Time                      :: " << std::setw(16) << static_cast<double>(tmpTime[5]/1000)/1000000                            << " [s],  Operation Last Number Of Calls to Load         :: " << std::setw(16) << tmpCalls[5]<< std::endl << std::endl;
    std::cout << " + Operation Tot. Init Time                      :: " << std::setw(16) << static_cast<double>(profileTime_[0]/1000)/1000000                       << " [s],  Operation Tot. Number Of Calls to Init         :: " << std::setw(16) << profileNCalls_[0]<< std::endl;
    std::cout << " + Operation Tot. UpdateData Time                :: " << std::setw(16) << static_cast<double>(profileTime_[1]/1000)/1000000                       << " [s],  Operation Tot. Number Of Calls to UpdateData   :: " << std::setw(16) << profileNCalls_[1]<< std::endl;
    std::cout << " + Operation Tot. UpdateWindow Time              :: " << std::setw(16) << static_cast<double>(profileTime_[2]/1000)/1000000                       << " [s],  Operation Tot. Number Of Calls to UpdateWindow :: " << std::setw(16) << profileNCalls_[2]<< std::endl;
    std::cout << " + Operation Tot. Compute Time                   :: " << std::setw(16) << static_cast<double>(profileTime_[3]/1000)/1000000                       << " [s],  Operation Tot. Number Of Calls to Compute      :: " << std::setw(16) << profileNCalls_[3]<< std::endl;
    std::cout << " + Operation Tot. Dump Time                      :: " << std::setw(16) << static_cast<double>(profileTime_[4]/1000)/1000000                       << " [s],  Operation Tot. Number Of Calls to Dump         :: " << std::setw(16) << profileNCalls_[4]<< std::endl;
    std::cout << " + Operation Tot. Load Time                      :: " << std::setw(16) << static_cast<double>(profileTime_[5]/1000)/1000000                       << " [s],  Operation Tot. Number Of Calls to Load         :: " << std::setw(16) << profileNCalls_[5]<< std::endl<< std::endl;
    std::cout << " + Operation Mean Init Time                      :: " << std::setw(16) << static_cast<double>(profileTime_[0]/1000)/(nExecuteImplCalls_*1000000)  << " [s]" << std::endl;
    std::cout << " + Operation Mean UpdateData Time                :: " << std::setw(16) << static_cast<double>(profileTime_[1]/1000)/(nExecuteImplCalls_*1000000)  << " [s]" << std::endl;
    std::cout << " + Operation Mean UpdateWindow Time              :: " << std::setw(16) << static_cast<double>(profileTime_[2]/1000)/(nExecuteImplCalls_*1000000)  << " [s]" << std::endl;
    std::cout << " + Operation Mean Compute Time                   :: " << std::setw(16) << static_cast<double>(profileTime_[3]/1000)/(nExecuteImplCalls_*1000000)  << " [s]" << std::endl;
    std::cout << " + Operation Mean Dump Time                      :: " << std::setw(16) << static_cast<double>(profileTime_[4]/1000)/(nExecuteImplCalls_*1000000)  << " [s]" << std::endl;
    std::cout << " + Operation Mean Load Time                      :: " << std::setw(16) << static_cast<double>(profileTime_[5]/1000)/(nExecuteImplCalls_*1000000)  << " [s]" << std::endl << std::endl;

    std::cout << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -" << std::endl;

    std::cout << " + ExecuteImpl. Last Flush Time                  :: " << std::setw(16) << static_cast<double>(tmpTime[6]/1000)/1000000                            << " [s],  ExecuteImpl. Last Number Of Calls to Flush                  " << std::setw(16) << tmpCalls[6]<< std::endl; 
    std::cout << " + ExecuteImpl. Last Key Generation Time         :: " << std::setw(16) << static_cast<double>(tmpTime[7]/1000)/1000000                            << " [s],  ExecuteImpl. Last Number Of Calls to Key Generation         " << std::setw(16) << tmpCalls[7]<< std::endl;
    std::cout << " + ExecuteImpl. Last Search Time                 :: " << std::setw(16) << static_cast<double>(tmpTime[8]/1000)/1000000                            << " [s],  ExecuteImpl. Last Number Of Calls to Search                 " << std::setw(16) << tmpCalls[8]<< std::endl;
    std::cout << " + ExecuteImpl. Last Create Time                 :: " << std::setw(16) << static_cast<double>(tmpTime[9]/1000)/1000000                            << " [s],  ExecuteImpl. Last Number Of Calls to Create                 " << std::setw(16) << tmpCalls[9]<< std::endl;
    std::cout << " + ExecuteImpl. Last UpdateData Time             :: " << std::setw(16) << static_cast<double>(tmpTime[10]/1000)/1000000                           << " [s],  ExecuteImpl. Last Number Of Calls to UpdateData             " << std::setw(16) << tmpCalls[10]<< std::endl;
    std::cout << " + ExecuteImpl. Last UdateGeneralMetadata Time   :: " << std::setw(16) << static_cast<double>(tmpTime[11]/1000)/1000000                           << " [s],  ExecuteImpl. Last Number Of Calls to UdateGeneralMetadata   " << std::setw(16) << tmpCalls[11]<< std::endl;
    std::cout << " + ExecuteImpl. Last OutputBufferAllocaton Time  :: " << std::setw(16) << static_cast<double>(tmpTime[12]/1000)/1000000                           << " [s],  ExecuteImpl. Last Number Of Calls to OutputBufferAllocaton  " << std::setw(16) << tmpCalls[12]<< std::endl;
    std::cout << " + ExecuteImpl. Last UpdateSynopticMetadata Time :: " << std::setw(16) << static_cast<double>(tmpTime[13]/1000)/1000000                           << " [s],  ExecuteImpl. Last Number Of Calls to UpdateSynopticMetadata " << std::setw(16) << tmpCalls[13]<< std::endl;
    std::cout << " + ExecuteImpl. Last Compute Time                :: " << std::setw(16) << static_cast<double>(tmpTime[14]/1000)/1000000                           << " [s],  ExecuteImpl. Last Number Of Calls to Compute                " << std::setw(16) << tmpCalls[14]<< std::endl;
    std::cout << " + ExecuteImpl. Last UpdateWindow Time           :: " << std::setw(16) << static_cast<double>(tmpTime[15]/1000)/1000000                           << " [s],  ExecuteImpl. Last Number Of Calls to UpdateWindow           " << std::setw(16) << tmpCalls[15]<< std::endl << std::endl;
    std::cout << " + ExecuteImpl. Tot. Flush Time                  :: " << std::setw(16) << static_cast<double>(profileTime_[6]/1000)/1000000                       << " [s],  ExecuteImpl. Tot. Number Of Calls to Flush                  " << std::setw(16) << profileNCalls_[6]<< std::endl; 
    std::cout << " + ExecuteImpl. Tot. Key Generation Time         :: " << std::setw(16) << static_cast<double>(profileTime_[7]/1000)/1000000                       << " [s],  ExecuteImpl. Tot. Number Of Calls to Key Generation         " << std::setw(16) << profileNCalls_[7]<< std::endl;
    std::cout << " + ExecuteImpl. Tot. Search Time                 :: " << std::setw(16) << static_cast<double>(profileTime_[8]/1000)/1000000                       << " [s],  ExecuteImpl. Tot. Number Of Calls to Search                 " << std::setw(16) << profileNCalls_[8]<< std::endl;
    std::cout << " + ExecuteImpl. Tot. Create Time                 :: " << std::setw(16) << static_cast<double>(profileTime_[9]/1000)/1000000                       << " [s],  ExecuteImpl. Tot. Number Of Calls to Create                 " << std::setw(16) << profileNCalls_[9]<< std::endl;
    std::cout << " + ExecuteImpl. Tot. UpdateData Time             :: " << std::setw(16) << static_cast<double>(profileTime_[10]/1000)/1000000                      << " [s],  ExecuteImpl. Tot. Number Of Calls to UpdateData             " << std::setw(16) << profileNCalls_[10]<< std::endl;
    std::cout << " + ExecuteImpl. Tot. UdateGeneralMetadata Time   :: " << std::setw(16) << static_cast<double>(profileTime_[11]/1000)/1000000                      << " [s],  ExecuteImpl. Tot. Number Of Calls to UdateGeneralMetadata   " << std::setw(16) << profileNCalls_[11]<< std::endl;
    std::cout << " + ExecuteImpl. Tot. OutputBufferAllocaton Time  :: " << std::setw(16) << static_cast<double>(profileTime_[12]/1000)/1000000                      << " [s],  ExecuteImpl. Tot. Number Of Calls to OutputBufferAllocaton  " << std::setw(16) << profileNCalls_[12]<< std::endl;
    std::cout << " + ExecuteImpl. Tot. UpdateSynopticMetadata Time :: " << std::setw(16) << static_cast<double>(profileTime_[13]/1000)/1000000                      << " [s],  ExecuteImpl. Tot. Number Of Calls to UpdateSynopticMetadata " << std::setw(16) << profileNCalls_[13]<< std::endl;
    std::cout << " + ExecuteImpl. Tot. Compute Time                :: " << std::setw(16) << static_cast<double>(profileTime_[14]/1000)/1000000                      << " [s],  ExecuteImpl. Tot. Number Of Calls to Compute                " << std::setw(16) << profileNCalls_[14]<< std::endl;
    std::cout << " + ExecuteImpl. Tot. UpdateWindow Time           :: " << std::setw(16) << static_cast<double>(profileTime_[15]/1000)/1000000                      << " [s],  ExecuteImpl. Tot. Number Of Calls to UpdateWindow           " << std::setw(16) << profileNCalls_[15]<< std::endl << std::endl;
    std::cout << " + ExecuteImpl. Mean Flush Time                  :: " << std::setw(16) << static_cast<double>(profileTime_[6]/1000)/ (nExecuteImplCalls_*1000000) << " [s]" << std::endl; 
    std::cout << " + ExecuteImpl. Mean Key Generation Time         :: " << std::setw(16) << static_cast<double>(profileTime_[7]/1000)/ (nExecuteImplCalls_*1000000) << " [s]" << std::endl;
    std::cout << " + ExecuteImpl. Mean Search Time                 :: " << std::setw(16) << static_cast<double>(profileTime_[8]/1000)/ (nExecuteImplCalls_*1000000) << " [s]" << std::endl;
    std::cout << " + ExecuteImpl. Mean Create Time                 :: " << std::setw(16) << static_cast<double>(profileTime_[9]/1000)/ (nExecuteImplCalls_*1000000) << " [s]" << std::endl;
    std::cout << " + ExecuteImpl. Mean UpdateData Time             :: " << std::setw(16) << static_cast<double>(profileTime_[10]/1000)/(nExecuteImplCalls_*1000000) << " [s]" << std::endl;
    std::cout << " + ExecuteImpl. Mean UdateGeneralMetadata Time   :: " << std::setw(16) << static_cast<double>(profileTime_[11]/1000)/(nExecuteImplCalls_*1000000) << " [s]" << std::endl;
    std::cout << " + ExecuteImpl. Mean OutputBufferAllocaton Time  :: " << std::setw(16) << static_cast<double>(profileTime_[12]/1000)/(nExecuteImplCalls_*1000000) << " [s]" << std::endl;
    std::cout << " + ExecuteImpl. Mean UpdateSynopticMetadata Time :: " << std::setw(16) << static_cast<double>(profileTime_[13]/1000)/(nExecuteImplCalls_*1000000) << " [s]" << std::endl;
    std::cout << " + ExecuteImpl. Mean Compute Time                :: " << std::setw(16) << static_cast<double>(profileTime_[14]/1000)/(nExecuteImplCalls_*1000000) << " [s]" << std::endl;
    std::cout << " + ExecuteImpl. Mean UpdateWindow Time           :: " << std::setw(16) << static_cast<double>(profileTime_[15]/1000)/(nExecuteImplCalls_*1000000) << " [s]" << std::endl << std::endl;
    std::cout << "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -" << std::endl;
    
    double OpTime_i = static_cast<double>((tmpTime[0] + tmpTime[1] + tmpTime[2] + tmpTime[3]));
    double XiTime_i = static_cast<double>((tmpTime[7] + tmpTime[8] + tmpTime[9] + tmpTime[10] + tmpTime[11] + tmpTime[12] + tmpTime[13] + tmpTime[14] + tmpTime[15]));

    double OpTime_m = static_cast<double>((profileTime_[0] + profileTime_[1] + profileTime_[2] + profileTime_[3]));
    double XiTime_m = static_cast<double>((profileTime_[7] + profileTime_[8] + profileTime_[9] + profileTime_[10] + profileTime_[11] + profileTime_[12] + profileTime_[13] + profileTime_[14] + profileTime_[15]));

    std::cout << " + Last floating point intensity                 :: " << OpTime_i/XiTime_i << ", Mean floating point intensity    :: " << OpTime_m/XiTime_m << std::endl;
    std::cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++" << std::endl;

    return;
}


std::string Statistics::generateKey(const message::Message& msg) const {
    std::ostringstream os;
    os << msg.metadata().getString("param", "") << "-" << msg.metadata().getString("paramId", "") << "-"
       << msg.metadata().getLong("level", 0) << "-" << msg.metadata().getLong("levelist", 0) << "-"
       << msg.metadata().getString("levtype", "unknown") << "-" << msg.metadata().getString("gridType", "unknown")
       << "-" << msg.metadata().getString("precision", "unknown") << "-"
       << std::to_string(std::hash<std::string>{}(msg.source()));
    LOG_DEBUG_LIB(LibMultio) << "Generating key for the field :: " << os.str() << std::endl;
    return os.str();
}

// Message(version=1, tag=Field, source=Peer(group=null,id=18446744073709551615), destination=Peer(group=null,id=18446744073709551615), metadata={"domain":"g","date":20200120,"time":0,"expver":"hvi1","class":"rd","type":"fc","stream":"lwda","anoffset":9,"step":1,"levelist":1,"levtype":"ml","param":152,"gridType":"reduced_gg","name":"Logarithm of surface pressure","shortName":"lnsp","paramId":20,"gribEdition":"2","level":100,"timeStep":3600,"step-frequency":1,"globalSize":1000000,"stepId":1,"precision":"double"}, payload-size=8000000)


message::Metadata Statistics::outputMetadata(const message::Metadata& inputMetadata, const StatisticsConfiguration& cfg,
                                             const std::string& key) const {
    auto& win = fieldStats_.at(key)->cwin();
    if (win.endPointInSeconds() % 3600 != 0L) {
        std::ostringstream os;
        os << "Step in seconds needs to be a multiple of 3600 :: " << fieldStats_.at(key)->win().endPointInSeconds()
           << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    auto md = inputMetadata;
    md.set("timeUnit", periodUpdater_->timeUnit());
    md.set("startDate", win.epochPoint().date().yyyymmdd());
    md.set("startTime", win.epochPoint().time().hhmmss());
    md.set("timeSpanInHours", win.timeSpanInHours());
    md.set("stepRange", win.stepRange());
    md.set("previousDate", win.creationPoint().date().yyyymmdd());
    md.set("previousTime", win.creationPoint().time().hhmmss());
    md.set("currentDate", win.endPoint().date().yyyymmdd());
    md.set("currentTime", win.endPoint().time().hhmmss());
    md.set("stepInHours", win.endPointInHours());
    md.set("stepRangeInHours", win.stepRangeInHours());
    return md;
}


void Statistics::executeImpl(message::Message msg) {

    if (msg.tag() == message::Message::Tag::Flush) {
        profiler_[0].tic();
        nFlushCalls_++;
        DumpRestart();
        profiler_[0].toc();
        PrintProfilingInfo();
        executeNext(msg);
        return;
    }


    if (msg.tag() != message::Message::Tag::Field) {
        executeNext(msg);
        return;
    }

    profiler_[1].tic();
    nExecuteImplCalls_++;
    std::string key = generateKey(msg);
    StatisticsConfiguration cfg{cfg_, msg};
    IOmanager_->reset();
    IOmanager_->setCurrStep(cfg.restartStep());
    IOmanager_->setKey(key);
    profiler_[1].toc();


    profiler_[2].tic();
    util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
    auto stat = fieldStats_.find(key);
    profiler_[2].toc();


    profiler_[3].tic();
    if ( stat == fieldStats_.end()) {
        fieldStats_[key]
            = std::make_unique<TemporalStatistics>(periodUpdater_, operations_, msg, IOmanager_, matcherCfg_, cfg);
        if (cfg.solver_send_initial_condition()) {
            util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
            profiler_[3].toc( );
            return;
        }
        else {
            stat = fieldStats_.find(key);
        }
    }
    profiler_[3].toc();


    profiler_[4].tic();
    stat->second->updateData(msg, cfg);
    profiler_[4].toc();


    if (stat->second->isEndOfWindow(msg, cfg)) {

        profiler_[5].tic();
        auto md = outputMetadata(msg.metadata(), cfg, key);
        profiler_[5].toc();

        for (auto it = stat->second->collection_begin(); it != stat->second->collection_end(); ++it) {

            profiler_[6].tic();
            eckit::Buffer payload;
            payload.resize((*it)->byte_size());
            payload.zero();
            profiler_[6].toc( );

            profiler_[7].tic();
            md.set("operation", (*it)->operation());
            profiler_[7].toc();

            profiler_[8].tic();    
            (*it)->compute(payload);
            profiler_[8].toc();    

            executeNext(message::Message{message::Message::Header{message::Message::Tag::Field, msg.source(),
                        msg.destination(),
                        message::Metadata{md}},
                        std::move(payload)});
      
        }

        profiler_[9].tic();
        util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
        fieldStats_.at(key)->updateWindow(msg, cfg);
        profiler_[9].toc();
    }
    
    return;
}


void Statistics::print(std::ostream& os) const {
    os << "Statistics(output frequency = " << periodUpdater_->timeSpan() << ", unit = " << periodUpdater_->timeUnit()
       << ", operations = ";
    bool first = true;
    for (const auto& ops : operations_) {
        os << (first ? "" : ", ");
        os << ops;
        first = false;
    }
    os << ")";
}


static ActionBuilder<Statistics> StatisticsBuilder("statistics");

}  // namespace multio::action
