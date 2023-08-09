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
}


Statistics::Statistics(const ComponentConfiguration& compConf) :
    ChainedAction{compConf},
    cfg_{compConf},
    operations_{compConf.parsedConfig().getStringVector("operations")},
    periodUpdater_{make_period_updater(compConf.parsedConfig().getString("output-frequency"))},
    IOmanager_{StatisticsIOFactory::instance().build(cfg_.restartLib(), cfg_.restartPath(), cfg_.restartPrefix())},
    matcherCfg_{getMatcherConfiguration(compConf)},
    profiler_{} {}


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
    timingData_.updateCounter();
    timingData_.reset();
    for (auto ite = fieldStats_.begin(); ite != fieldStats_.end(); ite++) {
        LOG_DEBUG_LIB(LibMultio) << " + Collect prfiling info " << std::endl;
        for (auto it = ite->second->collection_begin(); it != ite->second->collection_end(); ++it) {
            LOG_DEBUG_LIB(LibMultio) << " + Print profiling info related to operations" << std::endl;
            timingData_.InitTime( (*it)->getTotalTimeNsec( 0 ) );
            timingData_.DataTime( (*it)->getTotalTimeNsec( 1 ) );
            timingData_.WindowTime ( (*it)->getTotalTimeNsec( 2 ) );
            timingData_.ComputeTime( (*it)->getTotalTimeNsec( 3 ) );
            timingData_.DumpTime( (*it)->getTotalTimeNsec( 4 ) );
            timingData_.LoadTime( (*it)->getTotalTimeNsec( 5 ) );
            timingData_.TotalMemory( (*it)->memory_in_bytes()     );
        }
    }
    

    std::cout << "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++" << std::endl;
    std::cout << " + Cnt              :: " << timingData_.cnt()             << std::endl;
    std::cout << " + InitTime         :: " << static_cast<double>(timingData_.TotInitTime()/1000)/1000000     << std::endl;
    std::cout << " + UpdateWindowTime :: " << static_cast<double>(timingData_.TotWindowTime()/1000)/1000000   << std::endl;
    std::cout << " + UpdateDataTime   :: " << static_cast<double>(timingData_.TotDataTime()/1000)/1000000     << std::endl;
    std::cout << " + ComputeTime      :: " << static_cast<double>(timingData_.TotComputeTime()/1000)/1000000  << std::endl;
    std::cout << " + Dump Time        :: " << static_cast<double>(timingData_.TotDumpTime()/1000)/1000000     << std::endl;
    std::cout << " + Load Time        :: " << static_cast<double>(timingData_.TotLoadTime()/1000)/1000000     << std::endl;
    std::cout << " + TotMemory        :: " << static_cast<double>(timingData_.TotalMemory()/1000)/1000000     << std::endl;

    std::cout << " + Time to Flush   :: " << static_cast<double>(profiler_.getTotalTimeNsec( 0 )/1000000)/1000 << ", (" << profiler_.getNumberOfCalls(0) << ")" << std::endl;
    std::cout << " + Time to forward :: " << static_cast<double>(profiler_.getTotalTimeNsec( 1 )/1000000)/1000 << ", (" << profiler_.getNumberOfCalls(1) << ")" << std::endl;
    std::cout << " + Time to create  :: " << static_cast<double>(profiler_.getTotalTimeNsec( 2 )/1000000)/1000 << ", (" << profiler_.getNumberOfCalls(2) << ")" << std::endl;
    std::cout << " + Time to compute :: " << static_cast<double>(profiler_.getTotalTimeNsec( 3 )/1000000)/1000 << ", (" << profiler_.getNumberOfCalls(3) << ")" << std::endl;

    double tmp = ( static_cast<double>(timingData_.TotInitTime()/1000)/1000000 + static_cast<double>(timingData_.TotWindowTime()/1000)/1000000 +
                   static_cast<double>(timingData_.TotDataTime()/1000)/1000000 + static_cast<double>(timingData_.TotComputeTime()/1000)/1000000 ) /
                   (static_cast<double>(profiler_.getTotalTimeNsec( 3 )/1000)/1000000);
    std::cout << " + Floating point intensity :: " << tmp << ", (" << profiler_.getNumberOfCalls(3) << ")" << std::endl;
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

    profiler_.tic();
    if (msg.tag() == message::Message::Tag::Flush) {
        DumpRestart();
        PrintProfilingInfo();
        profiler_.toc( 0 );
        executeNext(msg);
        return;
    }

    if (msg.tag() != message::Message::Tag::Field) {
        profiler_.toc( 1 );
        executeNext(msg);
        return;
    }

    std::string key = generateKey(msg);
    StatisticsConfiguration cfg{cfg_, msg};
    IOmanager_->reset();
    IOmanager_->setCurrStep(cfg.restartStep());
    IOmanager_->setKey(key);


    util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

    if (fieldStats_.find(key) == fieldStats_.end()) {
        fieldStats_[key]
            = std::make_unique<TemporalStatistics>(periodUpdater_, operations_, msg, IOmanager_, matcherCfg_, cfg);
        if (cfg.solver_send_initial_condition()) {
            util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
            profiler_.toc( 2 );
            return;
        }
    }

    fieldStats_.at(key)->updateData(msg, cfg);

    if (fieldStats_.at(key)->isEndOfWindow(msg, cfg)) {
        auto md = outputMetadata(msg.metadata(), cfg, key);
        for (auto it = fieldStats_.at(key)->collection_begin(); it != fieldStats_.at(key)->collection_end(); ++it) {
            eckit::Buffer payload;
            payload.resize((*it)->byte_size());
            payload.zero();
            md.set("operation", (*it)->operation());
            (*it)->compute(payload);
            profiler_.toc( 3 );
            executeNext(message::Message{message::Message::Header{message::Message::Tag::Field, msg.source(),
                        msg.destination(),
                        message::Metadata{md}},
                        std::move(payload)});
            profiler_.tic();        
        }

        util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

        fieldStats_.at(key)->updateWindow(msg, cfg);
    }


    profiler_.toc( 3 );    
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
