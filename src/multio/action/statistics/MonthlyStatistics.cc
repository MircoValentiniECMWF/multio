#include "MonthlyStatistics.h"

#include "multio/action/statistics/Period.h"

namespace multio {
namespace action {


namespace {
eckit::DateTime computeMonthWinStartTime(const eckit::DateTime& currentTime) {
    auto year = currentTime.date().year();
    auto month = currentTime.date().month();
    return eckit::DateTime{eckit::Date{year, month, 1}, eckit::Time{0}};
};


eckit::DateTime computeMonthWinCreationTime(const eckit::DateTime& currentTime) {
    // Not set to the beginning of the month otherwise the step range is broken
    return currentTime;
};

eckit::DateTime updateMonthEnd(const eckit::DateTime& startPoint, long span) {
    auto totalSpan = startPoint.date().month() + span - 1;  // Make it zero-based
    auto endYear = startPoint.date().year() + totalSpan / 12;
    auto endMonth = totalSpan % 12 + 1;
    return eckit::DateTime{eckit::Date{endYear, endMonth, 1}, eckit::Time{0}};
};

eckit::DateTime computeMonthWinEndTime(const eckit::DateTime& startPoint, long span) {
    return eckit::DateTime{updateMonthEnd(startPoint, span)};
};

}  // namespace

DateTimePeriod setMonthlyPeriod(long span, const message::Message& msg, const std::string& partialPath,
                                const StatisticsOptions& options) {
    eckit::DateTime startPoint{computeMonthWinStartTime(winStartDateTime(msg, options))};
    eckit::DateTime creationPoint{computeMonthWinCreationTime(winStartDateTime(msg, options))};
    eckit::DateTime endPoint{computeMonthWinEndTime(startPoint, span)};
    return options.readRestart() ? DateTimePeriod{partialPath, "m", options}
                                 : DateTimePeriod{startPoint, creationPoint, endPoint, "m"};
}
MonthlyStatistics::MonthlyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                                     const std::string& partialPath, const StatisticsOptions& options) :
    TemporalStatistics{operations, setMonthlyPeriod(span, msg, partialPath, options), msg, partialPath, options, span} {
}

void MonthlyStatistics::resetPeriod(const message::Message& msg) {
    eckit::DateTime startPoint = computeMonthWinStartTime(currentDateTime(msg, options_));
    eckit::DateTime endPoint = updateMonthEnd(startPoint, span_);
    current_.reset(startPoint, endPoint);
};

void MonthlyStatistics::print(std::ostream& os) const {
    os << "Monthly Statistics(" << current_ << ")";
}

}  // namespace action
}  // namespace multio