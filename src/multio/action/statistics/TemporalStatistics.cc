#include "TemporalStatistics.h"

#include <algorithm>
#include <cstring>
#include <fstream>
#include <iostream>

#include "eckit/exception/Exceptions.h"
#include "multio/LibMultio.h"
#include "multio/util/PrecisionTag.h"

#include "DailyStatistics.h"
#include "HourlyStatistics.h"
#include "MonthlyStatistics.h"

namespace multio {
namespace action {

namespace {

auto reset_statistics(const std::vector<std::string>& opNames, message::Message msg, const std::string& partialPath,
                      const StatisticsOptions& options, bool restart) {
    // NOTE: in this case the lambda must catch everything
    return multio::util::dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        std::vector<OperationVar> stats;
        for (const auto& op : opNames) {
            stats.push_back(make_operation<Precision>(op, msg.size(), partialPath, options, restart));
        }
        return stats;
    });
}
}  // namespace

std::unique_ptr<TemporalStatistics> TemporalStatistics::build(const std::string& unit, long span,
                                                              const std::vector<std::string>& operations,
                                                              const message::Message& msg,
                                                              const std::string& partialPath,
                                                              const StatisticsOptions& options) {

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
                                       const StatisticsOptions& options, long span) :
    span_{span},
    name_{msg.name()},
    partialPath_{partialPath},
    prevStep_{options.step()},
    current_{period},
    options_{options},
    opNames_{operations},
    statistics_{reset_statistics(operations, msg, partialPath, options, options.readRestart())} {}


void TemporalStatistics::dump(const long step, const StatisticsOptions& options) const {
    LOG_DEBUG_LIB(LibMultio) << " [" << partialPath_ << "-" << step << " - " << options.logPrefix() << "] DUMP FILE"
                             << std::endl;
    std::string fname = partialPath_ + "-" + std::to_string(step);
    current_.dump(partialPath_, step);
    for (auto const& stat : statistics_) {
        std::visit(Overloaded{[this, step](const std::unique_ptr<Operation<double>>& arg) {
                                  return arg->dump(this->partialPath_, step);
                              },
                              [this, step](const std::unique_ptr<Operation<float>>& arg) {
                                  return arg->dump(this->partialPath_, step);
                              }},
                   stat);
    }
    return;
}


bool TemporalStatistics::process(message::Message& msg, const StatisticsOptions& options) {
    options_ = options;
    return process_next(msg);
}

void TemporalStatistics::updateStatistics(const message::Message& msg) {
    for (auto const& stat : statistics_) {
        std::visit(Overloaded{[&msg](const std::unique_ptr<Operation<double>>& arg) {
                                  return arg->update(msg.payload().data(), msg.size());
                              },
                              [&msg](const std::unique_ptr<Operation<float>>& arg) {
                                  return arg->update(msg.payload().data(), msg.size());
                              }},
                   stat);
    }
}

bool TemporalStatistics::process_next(message::Message& msg) {
    if (name_ != msg.name()) {
        std::ostringstream os;
        os << "Name :: (" << name_ << ") of the current statistics is different from the name in the message :: ("
           << msg.name() << ")" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }

    LOG_DEBUG_LIB(multio::LibMultio) << *this << std::endl;
    LOG_DEBUG_LIB(multio::LibMultio) << options_.logPrefix() << " [" << partialPath_ << "] *** Curr ";

    auto dateTime = currentDateTime(msg, options_);
    if (!current_.isWithin(dateTime)) {
        std::ostringstream os;
        os << options_.logPrefix() << dateTime << " is outside of current period " << current_ << std::endl;
        throw eckit::UserError(os.str(), Here());
    }

    updateStatistics(msg);

    LOG_DEBUG_LIB(multio::LibMultio) << options_.logPrefix() << " [" << partialPath_ << "] *** Next ";
    return current_.isWithin(nextDateTime(msg, options_));
}

std::string TemporalStatistics::stepRange(long step) {
    auto ret = std::to_string(prevStep_) + "-" + std::to_string(step);
    prevStep_ = step;
    LOG_DEBUG_LIB(multio::LibMultio) << options_.logPrefix() << " *** Setting step range: " << ret << std::endl;
    return ret;
}

const DateTimePeriod& TemporalStatistics::current() const {
    return current_;
}

void TemporalStatistics::reset(const message::Message& msg) {
    statistics_ = reset_statistics(opNames_, msg, partialPath_, options_, false);
    resetPeriod(msg);
    LOG_DEBUG_LIB(::multio::LibMultio) << options_.logPrefix() << " [" << partialPath_
                                       << "] ------ Resetting statistics for temporal type " << *this << std::endl;
}

std::map<std::string, eckit::Buffer> TemporalStatistics::compute(const message::Message& msg) {
    std::map<std::string, eckit::Buffer> retStats;
    for (auto const& stat : statistics_) {

        auto buf = std::visit([](auto&& arg) { return arg->compute(); }, stat);

        const auto& operation = std::visit([](auto&& arg) { return arg->operation(); }, stat);

        retStats.emplace(operation, std::move(buf));
    }
    return retStats;
}

}  // namespace action
}  // namespace multio
