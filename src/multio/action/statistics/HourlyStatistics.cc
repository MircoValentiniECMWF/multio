#include "HourlyStatistics.h"


#include "multio/action/statistics/Period.h"

namespace multio {
namespace action {


namespace {

eckit::DateTime computeHourWinStartTime(const eckit::DateTime& currentTime) {
    // Not set to the beginning of the day otherwise the step range is broken
    return eckit::DateTime{currentTime.date(), eckit::Time{currentTime.time().hours(), 0, 0}};
};

eckit::DateTime computeHourWinCreationTime(const eckit::DateTime& currentTime) {
    // Not set to the beginning of the hour otherwise the step range is broken
    return currentTime;
};

eckit::DateTime updateHourEnd(const eckit::DateTime& startPoint, long span) {
    eckit::DateTime tmp = startPoint + static_cast<eckit::Second>(3600 * span);
    return eckit::DateTime{tmp.date(), eckit::Time{tmp.time().hours(), 0, 0}};
};

eckit::DateTime computeHourWinEndTime(const eckit::DateTime& startPoint, long span) {
    return eckit::DateTime{updateHourEnd(startPoint, span)};
};

}  // namespace

DateTimePeriod setHourlyPeriod(long span, const message::Message& msg, const std::string& partialPath,
                               const StatisticsOptions& options) {
    eckit::DateTime startPoint{computeHourWinStartTime(winStartDateTime(msg, options))};
    eckit::DateTime creationPoint{computeHourWinCreationTime(winStartDateTime(msg, options))};
    eckit::DateTime endPoint{computeHourWinEndTime(startPoint, span)};
    return options.readRestart() ? DateTimePeriod{partialPath, "h", options}
                                 : DateTimePeriod{startPoint, creationPoint, endPoint, "h"};
}

HourlyStatistics::HourlyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                                   const std::string& partialPath, const StatisticsOptions& options) :
    TemporalStatistics{operations, setHourlyPeriod(span, msg, partialPath, options), msg, partialPath, options, span} {
    // Restart constructor
}

void HourlyStatistics::resetPeriod(const message::Message& msg) {
    eckit::DateTime startPoint = computeHourWinStartTime(currentDateTime(msg, options_));
    eckit::DateTime endPoint = updateHourEnd(startPoint, span_);
    current_.reset(startPoint, endPoint);
};

void HourlyStatistics::print(std::ostream& os) const {
    os << "Hourly Statistics(" << current_ << ")";
}

}  // namespace action
}  // namespace multio