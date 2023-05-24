#include "DailyStatistics.h"

#include "multio/action/statistics/Period.h"

namespace multio {
namespace action {


namespace {
eckit::DateTime computeDayWinStartTime(const eckit::DateTime& currentTime) {
    // Not set to the beginning of the day otherwise the step range is broken
    return eckit::DateTime{currentTime.date(), eckit::Time{0}};
};

eckit::DateTime computeDayWinCreationTime(const eckit::DateTime& currentTime) {
    // Not set to the beginning of the day otherwise the step range is broken
    return currentTime;
};

eckit::DateTime updateDayEnd(const eckit::DateTime& startPoint, long span) {
    eckit::DateTime tmp = startPoint + static_cast<eckit::Second>(3600 * 24 * span);
    return eckit::DateTime{tmp.date(), eckit::Time{0}};
};

eckit::DateTime computeDayWinEndTime(const eckit::DateTime& startPoint, long span) {
    return eckit::DateTime{updateDayEnd(startPoint, span)};
};

}  // namespace

DateTimePeriod setDailyPeriod(long span, const message::Message& msg, const std::string& partialPath,
                              const StatisticsOptions& options) {
    eckit::DateTime startPoint{computeDayWinStartTime(winStartDateTime(msg, options))};
    eckit::DateTime creationPoint{computeDayWinCreationTime(winStartDateTime(msg, options))};
    eckit::DateTime endPoint{computeDayWinEndTime(startPoint, span)};
    return options.readRestart() ? DateTimePeriod{partialPath, "d", options}
                                 : DateTimePeriod{startPoint, creationPoint, endPoint, "d"};
}


DailyStatistics::DailyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                                 const std::string& partialPath, const StatisticsOptions& options) :
    TemporalStatistics{operations, setDailyPeriod(span, msg, partialPath, options), msg, partialPath, options, span} {
    // Restart constructor
}
void DailyStatistics::resetPeriod(const message::Message& msg) {
    eckit::DateTime startPoint = computeDayWinStartTime(currentDateTime(msg, options_));
    eckit::DateTime endPoint = updateDayEnd(startPoint, span_);
    current_.reset(startPoint, endPoint);
};
void DailyStatistics::print(std::ostream& os) const {
    os << "Daily Statistics(" << current_ << ")";
}

}  // namespace action
}  // namespace multio