#include "MonthlyStatistics.h"

#include "Period.h"

namespace multio::action {


namespace {
eckit::DateTime computeMonthWinStartTime(const eckit::DateTime& currentTime) {
    auto year = currentTime.date().year();
    auto month = currentTime.date().month();
    return eckit::DateTime{eckit::Date{year, month, 1}, eckit::Time{0}};
};


eckit::DateTime computeMonthWinCreationTime(const eckit::DateTime& currentTime) {
    return currentTime;
};

eckit::DateTime updateMonthEnd(const eckit::DateTime& startPoint, long span) {
    auto totalSpan = startPoint.date().month() + span - 1;
    auto endYear = startPoint.date().year() + totalSpan / 12;
    auto endMonth = totalSpan % 12 + 1;
    return eckit::DateTime{eckit::Date{endYear, endMonth, 1}, eckit::Time{0}};
};

eckit::DateTime computeMonthWinEndTime(const eckit::DateTime& startPoint, long span) {
    return eckit::DateTime{updateMonthEnd(startPoint, span)};
};

}  // namespace

DateTimePeriod setMonthlyPeriod(long span, const message::Message& msg, const std::string& partialPath,
                                const StatisticsConfiguration& cfg) {
    eckit::DateTime epochPoint{epochDateTime(msg, cfg)};
    eckit::DateTime startPoint{computeMonthWinStartTime(winStartDateTime(msg, cfg))};
    eckit::DateTime creationPoint{computeMonthWinCreationTime(winStartDateTime(msg, cfg))};
    eckit::DateTime endPoint{computeMonthWinEndTime(startPoint, span)};
    return cfg.readRestart() ? DateTimePeriod{partialPath, "m", cfg}
                             : DateTimePeriod{epochPoint, startPoint, creationPoint, endPoint, cfg.timeStep(), "m"};
}
MonthlyStatistics::MonthlyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                                     const std::string& partialPath, const StatisticsConfiguration& cfg) :
    TemporalStatistics{operations, setMonthlyPeriod(span, msg, partialPath, cfg), msg, partialPath, cfg, span} {}

void MonthlyStatistics::resetPeriod(const message::Message& msg, const StatisticsConfiguration& cfg) {
    eckit::DateTime startPoint = computeMonthWinStartTime(currentDateTime(msg, cfg));
    eckit::DateTime endPoint = updateMonthEnd(startPoint, span_);
    current_.reset(startPoint, endPoint);
};

void MonthlyStatistics::print(std::ostream& os) const {
    os << "Monthly Statistics(" << current_ << ")";
}

}  // namespace multio::action