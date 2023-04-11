#include "TemporalStatistics.h"

#include <cstring>
#include <fstream>
#include <iostream>

#include "eckit/exception/Exceptions.h"
#include "multio/LibMultio.h"
#include "multio/util/PrecisionTag.h"

namespace multio {
namespace action {

namespace {
auto reset_statistics(const std::vector<std::string>& opNames, message::Message msg, const StatisticsOptions& options) {
    return multio::util::dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        std::vector<OperationVar> stats;
        for (const auto& op : opNames) {
            stats.push_back(make_operation<Precision>(op, msg.size(), options));
        }
        return stats;
    });
}

auto load_statistics(const std::vector<std::string>& opNames, message::Message msg, const std::string& key,
                     const StatisticsOptions& options) {
    return multio::util::dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        std::vector<OperationVar> stats;
        for (const auto& op : opNames) {
            stats.push_back(load_operation<Precision>(op, msg.size(), key, options));
        }
        return stats;
    });
}

eckit::DateTime currentDateTime(const message::Message& msg, const StatisticsOptions& options) {
    eckit::Date startDate{options.startDate()};
    long startTime = options.startTime();
    auto hour = startTime / 10000;
    auto minute = (startTime % 10000) / 100;
    eckit::DateTime startDateTime{startDate, eckit::Time{hour, minute, 0}};

    return startDateTime + static_cast<eckit::Second>(msg.metadata().getLong("step") * options.timeStep());
}

eckit::DateTime nextDateTime(const message::Message& msg, const StatisticsOptions& options) {
    return currentDateTime(msg, options) + static_cast<eckit::Second>(options.stepFreq() * options.timeStep());
}


}  // namespace

std::unique_ptr<TemporalStatistics> TemporalStatistics::build(const std::string& unit, long span,
                                                              const std::vector<std::string>& operations,
                                                              const message::Message& msg,
                                                              const StatisticsOptions& options) {
    long step = msg.metadata().getLong("step");

    if (unit == "month") {
        return std::make_unique<MonthlyStatistics>(operations, span, msg, options, step);
    }

    if (unit == "day") {
        return std::make_unique<DailyStatistics>(operations, span, msg, options, step);
    }

    if (unit == "hour") {
        return std::make_unique<HourlyStatistics>(operations, span, msg, options, step);
    }

    throw eckit::SeriousBug{"Temporal statistics for base period " + unit + " is not defined"};
}


std::unique_ptr<TemporalStatistics> TemporalStatistics::load(const std::string& unit, long span,
                                                             const std::vector<std::string>& operations,
                                                             const message::Message& msg, const std::string& key,
                                                             const StatisticsOptions& options) {

    // TODO: make some coherency checks
    long step = msg.metadata().getLong("step");
    std::cout << "Load a restart file" << std::endl;
    if (unit == "month") {
        return std::make_unique<MonthlyStatistics>(operations, span, msg, key, options, step);
    }

    if (unit == "day") {
        return std::make_unique<DailyStatistics>(operations, span, msg, key, options, step);
    }

    if (unit == "hour") {
        return std::make_unique<HourlyStatistics>(operations, span, msg, key, options, step);
    }

    throw eckit::SeriousBug{"Temporal statistics for base period " + unit + " is not defined"};
}


TemporalStatistics::TemporalStatistics(const std::vector<std::string>& operations, const DateTimePeriod& period,
                                       const message::Message& msg, const StatisticsOptions& options, long span,
                                       long step) :
    name_{msg.name()},
    current_{period},
    options_{options},
    opNames_{operations},
    statistics_{reset_statistics(operations, msg, options)},
    prevStep_{step},
    span_{span} {}


TemporalStatistics::TemporalStatistics(const std::vector<std::string>& operations, const DateTimePeriod& period,
                                       const message::Message& msg, const std::string& key,
                                       const StatisticsOptions& options, long span, long step) :
    name_{msg.name()},
    current_{period},
    options_{options},
    opNames_{operations},
    statistics_{load_statistics(operations, msg, key, options)},
    prevStep_{step},
    span_{span} {}


bool TemporalStatistics::process(message::Message& msg) {
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
    LOG_DEBUG_LIB(multio::LibMultio) << " *** Current ";

    auto dateTime = currentDateTime(msg, options_);
    if (!current_.isWithin(dateTime)) {
        std::ostringstream os;
        os << dateTime << " is outside of current period " << current_ << std::endl;
        throw eckit::UserError(os.str(), Here());
    }

    updateStatistics(msg);

    LOG_DEBUG_LIB(multio::LibMultio) << " *** Next    ";
    return current_.isWithin(nextDateTime(msg, options_));
}

void TemporalStatistics::resetPeriod(const message::Message& msg) {
    current_.reset(currentDateTime(msg, options_));
}


eckit::DateTime computeMonthStart(const eckit::DateTime& currentTime) {
    // Not set to the beginning of the month otherwise the step range is broken
    return currentTime;
};

eckit::DateTime updateMonthEnd(const eckit::DateTime& startPoint, long span) {
    auto totalSpan = startPoint.date().month() + span - 1;  // Make it zero-based
    auto endYear = startPoint.date().year() + totalSpan / 12;
    auto endMonth = totalSpan % 12 + 1;
    return eckit::DateTime{eckit::Date{endYear, endMonth, 1}, eckit::Time{0}};
};

bool isMonthStart(const eckit::DateTime& startPoint) {
    return (startPoint.date().day() == 1 && startPoint.time().hours() == 0 && startPoint.time().minutes() == 0
            && startPoint.time().seconds() == 0);
}

eckit::DateTime computeMonthEnd(const eckit::DateTime& startPoint, long span) {
    return eckit::DateTime{updateMonthEnd(startPoint, span)};
};

void MonthlyStatistics::resetPeriod(const message::Message& msg) {
    eckit::DateTime startPoint = computeMonthStart(currentDateTime(msg, options_));
    eckit::DateTime endPoint = updateMonthEnd(startPoint, span_);
    current_.reset(startPoint, endPoint);
};


eckit::DateTime computeDayStart(const eckit::DateTime& currentTime) {
    // Not set to the beginning of the day otherwise the step range is broken
    return currentTime;
};

eckit::DateTime updateDayEnd(const eckit::DateTime& startPoint, long span) {
    eckit::DateTime tmp = startPoint + eckit::Second{3600 * 24 * span};
    return eckit::DateTime{tmp.date(), eckit::Time{0}};
};

bool isDayStart(const eckit::DateTime& startPoint) {
    return (startPoint.time().hours() == 0 && startPoint.time().minutes() == 0 && startPoint.time().seconds() == 0);
}

eckit::DateTime computeDayEnd(const eckit::DateTime& startPoint, long span) {
    return eckit::DateTime{updateDayEnd(startPoint, span)};
};

void DailyStatistics::resetPeriod(const message::Message& msg) {
    eckit::DateTime startPoint = computeDayStart(currentDateTime(msg, options_));
    eckit::DateTime endPoint = updateDayEnd(startPoint, span_);
    current_.reset(startPoint, endPoint);
};


eckit::DateTime computeHourStart(const eckit::DateTime& currentTime) {
    // Not set to the beginning of the hour otherwise the step range is broken
    return currentTime;
};

eckit::DateTime updateHourEnd(const eckit::DateTime& startPoint, long span) {
    eckit::DateTime tmp = startPoint + eckit::Second{3600 * span};
    return eckit::DateTime{tmp.date(), eckit::Time{tmp.time().hours(), 0, 0}};
};

bool isHourStart(const eckit::DateTime& startPoint) {
    return (startPoint.time().minutes() == 0 && startPoint.time().seconds() == 0);
}

eckit::DateTime computeHourEnd(const eckit::DateTime& startPoint, long span) {
    return eckit::DateTime{updateHourEnd(startPoint, span)};
};

void HourlyStatistics::resetPeriod(const message::Message& msg) {
    eckit::DateTime startPoint = computeHourStart(currentDateTime(msg, options_));
    eckit::DateTime endPoint = updateHourEnd(startPoint, span_);
    current_.reset(startPoint, endPoint);
};


std::map<std::string, eckit::Buffer> TemporalStatistics::compute(const message::Message& msg) {
    std::map<std::string, eckit::Buffer> retStats;
    for (auto const& stat : statistics_) {

        auto buf = std::visit([](auto&& arg) { return arg->compute(); }, stat);

        const auto& operation = std::visit([](auto&& arg) { return arg->operation(); }, stat);

        retStats.emplace(operation, std::move(buf));
    }
    return retStats;
}

std::string TemporalStatistics::stepRange(long step) {
    auto ret = std::to_string(prevStep_) + "-" + std::to_string(step);
    prevStep_ = step;
    LOG_DEBUG_LIB(multio::LibMultio) << " *** Setting step range: " << ret << std::endl;
    return ret;
}

const DateTimePeriod& TemporalStatistics::current() const {
    return current_;
}

void TemporalStatistics::reset(const message::Message& msg) {
    statistics_ = reset_statistics(opNames_, msg, options_);
    resetPeriod(msg);
    LOG_DEBUG_LIB(::multio::LibMultio) << " ------ Resetting statistics for temporal type " << *this << std::endl;
}

//-------------------------------------------------------------------------------------------------

namespace {
DateTimePeriod setHourlyPeriod(long span, const message::Message& msg, const StatisticsOptions& options) {
    eckit::DateTime startPoint{computeHourStart(currentDateTime(msg, options))};
    eckit::DateTime endPoint{computeHourEnd(startPoint, span)};
    // First window is shorter, for this reason we need to pass the dimensions of a timestep in order to catch
    // the correct time span in hours
    return options.solver_send_initial_condition() ? DateTimePeriod{startPoint, endPoint}
                                   : DateTimePeriod{startPoint, endPoint, options.stepFreq() * options.timeStep()};
}
}  // namespace

HourlyStatistics::HourlyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                                   const StatisticsOptions& options, long step) :
    TemporalStatistics{operations, DateTimePeriod{setHourlyPeriod(span, msg, options)}, msg, options, span, step} {}

HourlyStatistics::HourlyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                                   const std::string& key, const StatisticsOptions& options, long step) :
    TemporalStatistics{operations, DateTimePeriod{"hourly"+key}, msg, "hourly-"+key, options, span, step} {
    // Restart constructor
}

void HourlyStatistics::dump(const std::string& name) const {
    std::ofstream dumpFile;
    std::ostringstream os;
    os << "hourly-" << name;
    std::string fname = os.str();
    current_.dump(fname);
    for (auto const& stat : statistics_) {
        std::visit(Overloaded{[&fname](const std::unique_ptr<Operation<double>>& arg) { return arg->dump(fname); },
                              [&fname](const std::unique_ptr<Operation<float>>& arg) { return arg->dump(fname); }},
                   stat);
    }
    return;
}

void HourlyStatistics::print(std::ostream& os) const {
    os << "Hourly Statistics(" << current_ << ")";
}

//-------------------------------------------------------------------------------------------------

namespace {
DateTimePeriod setDailyPeriod(long span, const message::Message& msg, const StatisticsOptions& options) {
    eckit::DateTime startPoint{computeDayStart(currentDateTime(msg, options))};
    eckit::DateTime endPoint{computeDayEnd(startPoint, span)};
    return options.solver_send_initial_condition()  ? DateTimePeriod{startPoint, endPoint}
                                                    : DateTimePeriod{startPoint, endPoint, options.stepFreq() * options.timeStep()};
}
}  // namespace


DailyStatistics::DailyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                                 const StatisticsOptions& options, long step) :
    TemporalStatistics{operations, DateTimePeriod{setDailyPeriod(span, msg, options)}, msg, options, span, step} {}

DailyStatistics::DailyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                                 const std::string& key, const StatisticsOptions& options, long step) :
    TemporalStatistics{operations, DateTimePeriod{"daily-"+key}, msg, "daily-"+key, options, span, step} {
    // Restart constructor

}

void DailyStatistics::dump(const std::string& name) const {
    std::ofstream dumpFile;
    std::ostringstream os;
    os << "daily-" << name;
    std::string fname = os.str();
    current_.dump(fname);
    for (auto const& stat : statistics_) {
        std::visit(Overloaded{[&fname](const std::unique_ptr<Operation<double>>& arg) { return arg->dump(fname); },
                              [&fname](const std::unique_ptr<Operation<float>>& arg) { return arg->dump(fname); }},
                   stat);
    }
    return;
}

void DailyStatistics::print(std::ostream& os) const {
    os << "Daily Statistics(" << current_ << ")";
}

//-------------------------------------------------------------------------------------------------

namespace {
DateTimePeriod setMonthlyPeriod(long span, const message::Message& msg, const StatisticsOptions& options) {
    eckit::DateTime startPoint{computeMonthStart(currentDateTime(msg, options))};
    eckit::DateTime endPoint{computeMonthEnd(startPoint, span)};
    return options.solver_send_initial_condition()  ? DateTimePeriod{startPoint, endPoint}
                                                    : DateTimePeriod{startPoint, endPoint, options.stepFreq() * options.timeStep()};
}
}  // namespace

#if 0
MonthlyStatistics::MonthlyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                                     const std::string& key, const StatisticsOptions& options, bool restart) :
    TemporalStatistics{operations, restart?DateTimePeriod{"monthly-"+key}:setMonthlyPeriod(span, msg, options), msg, "monthly-"+key, options, span, options.step(), restart} {}
#endif


MonthlyStatistics::MonthlyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                                     const StatisticsOptions& options, long step) :
    TemporalStatistics{operations, setMonthlyPeriod(span, msg, options), msg, options, span, step} {}


MonthlyStatistics::MonthlyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                                     const std::string& key, const StatisticsOptions& options, long step) :
    TemporalStatistics{operations, DateTimePeriod{"monthly-"+key}, msg, "monthly-"+key, options, span, step} {}


void MonthlyStatistics::dump(const std::string& name) const {
    std::ofstream dumpFile;
    std::ostringstream os;
    os << "monthly-" << name;
    std::string fname = os.str();
    current_.dump(fname);
    for (auto const& stat : statistics_) {
        std::visit(Overloaded{[&fname](const std::unique_ptr<Operation<double>>& arg) { return arg->dump(fname); },
                              [&fname](const std::unique_ptr<Operation<float>>& arg) { return arg->dump(fname); }},
                   stat);
    }
    return;
}


void MonthlyStatistics::print(std::ostream& os) const {
    os << "Monthly Statistics(" << current_ << ")";
}

//-------------------------------------------------------------------------------------------------

}  // namespace action
}  // namespace multio
