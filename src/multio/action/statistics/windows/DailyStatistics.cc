#include "DailyStatistics.h"

#include "Period.h"

namespace multio::action {

namespace {
eckit::DateTime computeDayWinStartTime(const eckit::DateTime& currentTime) {
    return eckit::DateTime{currentTime.date(), eckit::Time{0}};
};

eckit::DateTime computeDayWinCreationTime(const eckit::DateTime& currentTime) {
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
                              const StatisticsConfiguration& cfg) {
    eckit::DateTime epochPoint{epochDateTime(msg, cfg)};
    eckit::DateTime startPoint{computeDayWinStartTime(winStartDateTime(msg, cfg))};
    eckit::DateTime creationPoint{computeDayWinCreationTime(winStartDateTime(msg, cfg))};
    eckit::DateTime endPoint{computeDayWinEndTime(startPoint, span)};
    return cfg.readRestart() ? DateTimePeriod{partialPath, "d", cfg}
                             : DateTimePeriod{epochPoint, startPoint, creationPoint, endPoint, cfg.timeStep(), "d"};
}


DailyStatistics::DailyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                                 const std::string& partialPath, const StatisticsConfiguration& cfg) :
    TemporalStatistics{operations, setDailyPeriod(span, msg, partialPath, cfg), msg, partialPath, cfg, span} {
    // Restart constructor
}
void DailyStatistics::resetPeriod(const message::Message& msg, StatisticsConfiguration& cfg) {
    eckit::DateTime startPoint = computeDayWinStartTime(currentDateTime(msg, cfg));
    eckit::DateTime endPoint = updateDayEnd(startPoint, span_);
    current_.reset(startPoint, endPoint);
};
void DailyStatistics::print(std::ostream& os) const {
    os << "Daily Statistics(" << current_ << ")";
}

}  // namespace multio::action