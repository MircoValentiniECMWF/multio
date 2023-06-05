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

const std::map<const char, const std::string> symbol_to_unit{{'h', "hour"}, {'d', "day"}, {'m', "month"}};

std::string set_unit(std::string const& output_freq) {
    const auto& symbol = output_freq.back();

    if (symbol_to_unit.find(symbol) == end(symbol_to_unit)) {
        throw eckit::SeriousBug{"Time unit for symbol " + std::string{symbol} + " is not supported", Here()};
    }
    return symbol_to_unit.at(symbol);
}

long set_frequency(const std::string& output_freq) {
    auto freq = output_freq.substr(0, output_freq.size() - 1);
    return std::stol(freq);
}

}  // namespace

Statistics::Statistics(const ConfigurationContext& confCtx) :
    ChainedAction{confCtx},
    timeUnit_{set_unit(confCtx.config().getString("output-frequency"))},
    timeSpan_{set_frequency(confCtx.config().getString("output-frequency"))},
    operations_{confCtx.config().getStringVector("operations")},
    cfg_{confCtx.config()} {}


void Statistics::DumpRestart() const {
    try {
        std::shared_ptr<StatisticsIO> IOmanager{
            StatisticsIOFactory::instance().build("fstream_io", cfg_.restartPath(), cfg_.restartPrefix(), 0)};
        if (cfg_.writeRestart()) {
            LOG_DEBUG_LIB(LibMultio) << "Writing statistics checkpoint..." << std::endl;
            for (auto it = fieldStats_.begin(); it != fieldStats_.end(); it++) {
                LOG_DEBUG_LIB(LibMultio) << "Restart for field with key :: " << it->first << ", "
                                         << it->second->win().currPointInSteps() << std::endl;
                IOmanager->setStep(it->second->win().currPointInSteps());
                IOmanager->setKey(it->first);
                it->second->dump(IOmanager, cfg_);
            }
        }
    }
    catch (...) {
        std::ostringstream os;
        os << "Failed to write restart :: " << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
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


message::Metadata Statistics::outputMetadata(const message::Metadata& inputMetadata, const StatisticsConfiguration& cfg,
                                             const std::string& key) const {
    // Handling metadata
    if (fieldStats_.at(key)->win().endPointInSeconds() % 3600 != 0L) {
        std::ostringstream os;
        os << "Step in seconds needs to be a multiple of 3600 :: " << fieldStats_.at(key)->win().endPointInSeconds()
           << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    auto md = inputMetadata;
    md.set("timeUnit", timeUnit_);
    md.set("startDate", cfg.startDate());
    md.set("startTime", cfg.startTime());
    md.set("timeSpanInHours", fieldStats_.at(key)->win().timeSpanInHours());
    md.set("stepRange", fieldStats_.at(key)->win().stepRange());
    md.set("currentDate", fieldStats_.at(key)->win().endPoint().date().yyyymmdd());
    md.set("currentTime", fieldStats_.at(key)->win().endPoint().time().hhmmss());
    md.set("stepInHours", fieldStats_.at(key)->win().endPointInHours());
    md.set("stepRangeInHours", fieldStats_.at(key)->win().stepRangeInHours());
    return md;
}


void Statistics::executeImpl(message::Message msg) {

    // Pass through -- no statistics for messages other than fields
    if (msg.tag() != message::Message::Tag::Field) {
        if (msg.tag() == message::Message::Tag::Flush) {
            LOG_DEBUG_LIB(multio::LibMultio) << "statistics  :: Flush received" << std::endl;
            DumpRestart();
        }
        executeNext(msg);
        return;
    }

    // Local variables
    std::string key = generateKey(msg);
    StatisticsConfiguration cfg{cfg_, msg};
    std::shared_ptr<StatisticsIO> IOmanager{
        StatisticsIOFactory::instance().build("fstream_io", cfg.restartPath(), cfg.restartPrefix(), cfg.step())};
    IOmanager->setKey(key);

    {
        util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

        // Push new statistics
        LOG_DEBUG_LIB(multio::LibMultio) << "*** " << msg.destination() << " -- metadata: " << msg.metadata()
                                         << std::endl;
        if (fieldStats_.find(key) == fieldStats_.end()) {
            // Create a new statistics
            fieldStats_[key] = TemporalStatistics::build(timeUnit_, timeSpan_, operations_, msg, IOmanager, cfg);
            // Initial conditions don't need to be used in computation
            if (cfg.solver_send_initial_condition()) {
                LOG_DEBUG_LIB(LibMultio) << "Exiting because of initial condition :: " << key << std::endl;
                util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};
                return;
            }
        }

        fieldStats_.at(key)->updateData(msg, cfg);

        // Emit statistics
        if (fieldStats_.at(key)->isEndOfWindow(msg, cfg)) {

            // Construct output metadata
            auto md = outputMetadata(msg.metadata(), cfg, key);

            // Create outpuit messages
            for (auto it = fieldStats_.at(key)->begin(); it != fieldStats_.at(key)->end(); ++it) {
                eckit::Buffer payload;
                payload.resize((*it)->byte_size());
                payload.zero();
                md.set("operation", (*it)->operation());
                (*it)->compute(payload);
                executeNext(message::Message{message::Message::Header{message::Message::Tag::Field, msg.source(),
                                                                      msg.destination(), message::Metadata{md}},
                                             std::move(payload)});
            }

            // Timing
            util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

            // Reset operations and update window
            fieldStats_.at(key)->updateWindow(msg, cfg);
        }
    }
    return;
}

void Statistics::print(std::ostream& os) const {
    os << "Statistics(output frequency = " << timeSpan_ << ", unit = " << timeUnit_ << ", operations = ";
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
