#include "HourlyStatistics.h"

#include "Period.h"

namespace multio::action {

namespace {

eckit::DateTime computeHourWinStartTime(const eckit::DateTime& currentTime) {
    return eckit::DateTime{currentTime.date(), eckit::Time{currentTime.time().hours(), 0, 0}};
};

eckit::DateTime computeHourWinCreationTime(const eckit::DateTime& currentTime) {
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
                               const StatisticsConfiguration& cfg) {
    eckit::DateTime epochPoint{epochDateTime(msg, cfg)};
    eckit::DateTime startPoint{computeHourWinStartTime(winStartDateTime(msg, cfg))};
    eckit::DateTime creationPoint{computeHourWinCreationTime(winStartDateTime(msg, cfg))};
    eckit::DateTime endPoint{computeHourWinEndTime(startPoint, span)};
    return cfg.readRestart() ? DateTimePeriod{partialPath, "h", cfg}
                             : DateTimePeriod{epochPoint, startPoint, creationPoint, endPoint, cfg.timeStep(), "h"};
}

HourlyStatistics::HourlyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                                   const std::string& partialPath, const StatisticsConfiguration& cfg) :
    TemporalStatistics{operations, setHourlyPeriod(span, msg, partialPath, cfg), msg, partialPath, cfg, span} {
    // Restart constructor
}

void HourlyStatistics::resetPeriod(const message::Message& msg, const StatisticsConfiguration& cfg) {
    eckit::DateTime startPoint = computeHourWinStartTime(currentDateTime(msg, cfg));
    eckit::DateTime endPoint = updateHourEnd(startPoint, span_);
    current_.reset(startPoint, endPoint);
};

void HourlyStatistics::print(std::ostream& os) const {
    os << "Hourly Statistics(" << current_ << ")";
}

}  // namespace multio::action