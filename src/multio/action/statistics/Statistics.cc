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

namespace multio {
namespace action {

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
    options_{confCtx.config()} {}


Statistics::~Statistics(){
    // for (auto it = fieldStats_.begin(); it != fieldStats_.end(); it++) {
    //     // TODO: is key the correct value to use as prfix for file names.
    //     // is key constant across different software runs.
    //     it->second->dump(key);
    // }
}

std::string Statistics::getKey(const message::Message& msg) {
    std::ostringstream os;
    // TODO: "param" and "level" are needed. check and throw
    os << std::to_string(std::hash<std::string>{}(msg.metadata().getString("param", "unknown")))
       << std::to_string(std::hash<long>{}(msg.metadata().getLong("level", 999999999L)))
       << std::to_string(std::hash<std::string>{}(msg.source()));
    return os.str();
}

message::Metadata Statistics::outputMetadata(const message::Metadata& inputMetadata, const StatisticsOptions& opt,
                                             const std::string& key, long timeSpanInSeconds) const {
    // Handling metadata
    auto md = inputMetadata;
    md.set("timeUnit", timeUnit_);
    md.set("startDate", opt.startDate());
    md.set("startTime", opt.startTime());
    long timeSpanInHours = timeSpanInSeconds / 3600;
    md.set("timeSpanInHours", timeSpanInHours);
    md.set("stepRange", fieldStats_.at(key)->stepRange(md.getLong("step")));
    md.set("currentDate", fieldStats_.at(key)->current().endPoint().date().yyyymmdd());
    md.set("currentTime", fieldStats_.at(key)->current().endPoint().time().hhmmss());
    auto stepInSeconds = md.getLong("step") * opt.timeStep();
    std::cout << "The step is :: " << md.getLong("step") << std::endl;
    if (stepInSeconds % 3600 != 0L) {
        std::ostringstream os;
        os << "Step in seconds needs to be a multiple of 3600 :: " << stepInSeconds << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    auto stepInHours = stepInSeconds / 3600;
    std::cout << "The step (in hours) is :: " << stepInHours << std::endl;
    md.set("stepInHours", stepInHours);
    auto prevStep = std::max(stepInHours - timeSpanInHours, 0L);
    auto stepRangeInHours = std::to_string(prevStep) + "-" + std::to_string(stepInHours);
    std::cout << "The step range (in hours) is :: " << stepRangeInHours << std::endl;
    md.set("stepRangeInHours", stepRangeInHours);
    return md;
}

bool Statistics::restartExist(const std::string& key, const StatisticsOptions& opt) const {
    return true;
}

void Statistics::executeImpl(message::Message msg) {

    // Pass through -- no statistics for messages other than fields
    if (msg.tag() != message::Message::Tag::Field ) {
        executeNext(msg);
        return;
    }

    std::string key = getKey(msg);


    long timeSpanInSeconds;
    StatisticsOptions opt{options_, msg};

    if (!msg.metadata().has("step")) {
        throw eckit::UserError("Missing metadata \"step\"", Here());
    }

    {
        util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

        // Push new statistics
        LOG_DEBUG_LIB(multio::LibMultio) << "*** " << msg.destination() << " -- metadata: " << msg.metadata()
                                         << std::endl;
        if (fieldStats_.find(key) == end(fieldStats_)) {
            // Create a new statistics
            fieldStats_[key] = opt.restart() ? TemporalStatistics::load(timeUnit_, timeSpan_, operations_, msg, key, opt) : TemporalStatistics::build(timeUnit_, timeSpan_, operations_, msg, opt);
            // Initial conditions don't need to be used in computation (for the moment)
            if ( opt.solver_send_initial_condition()){
                return;
            }
        }

        // Time span needs to be computed here because otherwise it will be the timespan of the next window
        timeSpanInSeconds = fieldStats_.at(key)->current().timeSpanInSeconds();
        if (fieldStats_.at(key)->process(msg)) {
            return;
        }

        // Construct output metadata
        auto md = outputMetadata(msg.metadata(), opt, key, timeSpanInSeconds);

        // Emit finished statistics
        for (auto&& stat : fieldStats_.at(key)->compute(msg)) {
            md.set("operation", stat.first);
            message::Message newMsg{message::Message::Header{message::Message::Tag::Field, msg.source(),
                                                             msg.destination(), message::Metadata{md}},
                                    std::move(stat.second)};
            std::cout << "Exit span in seconds :: " << timeSpanInSeconds << std::endl;
            if (timeSpanInSeconds > 0) {
                executeNext(newMsg);
            }
            else {
                std::cout << "Rejected stats" << std::endl;
            }
        }
    }

    util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

    fieldStats_.at(key)->reset(msg);
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

}  // namespace action
}  // namespace multio
