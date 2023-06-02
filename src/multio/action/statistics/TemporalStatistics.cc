#include "TemporalStatistics.h"

#include <algorithm>
#include <cstring>
#include <fstream>
#include <iostream>

#include "eckit/exception/Exceptions.h"
#include "multio/LibMultio.h"
#include "multio/util/PrecisionTag.h"

#include "windows/DailyStatistics.h"
#include "windows/HourlyStatistics.h"
#include "windows/MonthlyStatistics.h"

namespace multio::action {

namespace {

auto build_statistics(const std::vector<std::string>& opNames, message::Message msg, const std::string& partialPath,
                      StatisticsOptions& options, bool restart) {
    return multio::util::dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        std::vector<std::unique_ptr<Operation>> stats;
        for (const auto& op : opNames) {
            stats.push_back(make_operation<Precision>(op, msg.size(), partialPath, options, restart));
            if ( options.solver_send_initial_condition() ){
                stats.back()->init(msg.payload().data(), msg.size(),currentDateTime(msg, options));
            }
            else {
                stats.back()->init(prevDateTime(msg, options));
            }
        }
        return stats;
    });
}
}  // namespace

std::unique_ptr<TemporalStatistics> TemporalStatistics::build(const std::string& unit, long span,
                                                              const std::vector<std::string>& operations,
                                                              const message::Message& msg,
                                                              const std::string& partialPath,
                                                              StatisticsOptions& options) {

    if (unit == "month") {
        return std::make_unique<MonthlyStatistics>(operations, span, msg, partialPath, options);
    }

    if (unit == "day") {
        return std::make_unique<DailyStatistics>(operations, span, msg, partialPath, options);
    }

    if (unit == "hour") {
        return std::make_unique<HourlyStatistics>(operations, span, msg, partialPath, options);
    }

    throw eckit::SeriousBug{"Temporal statistics for base period " + unit + " is not defined"};
}


TemporalStatistics::TemporalStatistics(const std::vector<std::string>& operations, const DateTimePeriod& period,
                                       const message::Message& msg, const std::string& partialPath,
                                       StatisticsOptions& options, long span) :
    span_{span},
    name_{msg.name()},
    partialPath_{partialPath},
    prevStep_{options.step()},
    current_{period},
    opNames_{operations},
    statistics_{build_statistics(operations, msg, partialPath, options, options.readRestart())} {}


void TemporalStatistics::dump(const long step, StatisticsOptions& options) const {
    LOG_DEBUG_LIB(LibMultio) << " [" << partialPath_ << "-" << step << " - " << options.logPrefix() << "] DUMP FILE"
                             << std::endl;
    current_.dump(partialPath_, options);
    for (auto& stat : statistics_) {
        stat->dump(partialPath_,step);
    }
    return;
}

void TemporalStatistics::update(message::Message& msg, StatisticsOptions& options) {
    checkName(msg.name());
    LOG_DEBUG_LIB(multio::LibMultio) << *this << std::endl;
    LOG_DEBUG_LIB(multio::LibMultio) << options.logPrefix() << " [" << partialPath_ << "] *** Curr ";

    checkIsWithin( msg, options );
    eckit::DateTime now = currentDateTime(msg, options);
    for (auto& stat : statistics_) {
        stat->update(msg.payload().data(), msg.size(), now);
    }

    LOG_DEBUG_LIB(multio::LibMultio) << options.logPrefix() << " [" << partialPath_ << "] *** Next ";
    return;
}

bool TemporalStatistics::isEndOfWindow(message::Message& msg, StatisticsOptions& options) {
    return !current_.isWithin(nextDateTime(msg, options));
}





std::string TemporalStatistics::stepRange(long step) {
    auto ret = std::to_string(prevStep_) + "-" + std::to_string(step);
    prevStep_ = step;
    // LOG_DEBUG_LIB(multio::LibMultio) << options_.logPrefix() << " *** Setting step range: " << ret << std::endl;
    return ret;
}

const DateTimePeriod& TemporalStatistics::current() const {
    return current_;
}

void TemporalStatistics::reset(const message::Message& msg, StatisticsOptions& options) {
    resetPeriod(msg,options);
    for (auto& stat : statistics_) {
        stat->reset(msg.payload().data(), msg.size(), currentDateTime(msg, options));
    }
    LOG_DEBUG_LIB(::multio::LibMultio) << options.logPrefix() << " [" << partialPath_
                                       << "] ------ Resetting statistics for temporal type " << *this << std::endl;
    return;
}

long TemporalStatistics::startStep() const { 
    return prevStep_;
};

void TemporalStatistics::checkName( const std::string& name ){
    if (name_ != name) {
        std::ostringstream os;
        os << "Name :: (" << name_ << ") of the current statistics is different from the name in the message :: ("
           << name << ")" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    return;
}

void TemporalStatistics::checkIsWithin( message::Message& msg, StatisticsOptions& options ){
    eckit::DateTime dateTime = currentDateTime(msg, options);
    if (!current_.isWithin(dateTime)) {
        std::ostringstream os;
        os << options.logPrefix() << dateTime << " is outside of current period " << current_ << std::endl;
        throw eckit::UserError(os.str(), Here());
    }
    return;
}

}
