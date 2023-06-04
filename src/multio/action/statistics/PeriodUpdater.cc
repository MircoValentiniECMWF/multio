#include "PeriodUpdater.h"

#include <iomanip>

#include "TimeUtils.h"

namespace multio::action {

PeriodUpdater::PeriodUpdater(long span) : span_{span} {};


eckit::DateTime PeriodUpdater::updatePeriodStart(const message::Message& msg, StatisticsConfiguration& cfg) {
    return computeWinStartTime(currentDateTime(msg, cfg));
};

eckit::DateTime PeriodUpdater::updatePeriodEnd(const message::Message& msg, StatisticsConfiguration& cfg) {
    return updateEnd(computeWinStartTime(currentDateTime(msg, cfg)), span_);
};

DateTimePeriod PeriodUpdater::initWindow(const message::Message& msg, const StatisticsIO& IOmanager,
                                         const StatisticsConfiguration& cfg) {
    if (cfg.readRestart()) {
        eckit::DateTime epochPoint{epochDateTime(msg, cfg)};
        eckit::DateTime startPoint{computeWinStartTime(winStartDateTime(msg, cfg))};
        eckit::DateTime creationPoint{computeWinCreationTime(winStartDateTime(msg, cfg))};
        eckit::DateTime endPoint{computeWinEndTime(startPoint, span_)};
        return MovingWindow{epochPoint, startPoint, creationPoint, endPoint, cfg.timeStep()};
    }
    else {
        return MovingWindow { IOmanager, cfg }
    }
}

eckit::DateTime PeriodUpdater::computeWinCreationTime(const eckit::DateTime& currentTime) {
    return currentTime;
};

eckit::DateTime PeriodUpdater::computeWinEndTime(const eckit::DateTime& startPoint) {
    return eckit::DateTime{updateWinEndTime(startPoint, span_)};
};


// -------------------------------------------------------------------------------------------------------------------

HourPeriodUpdater::HourPeriodUpdater(long span) : PeriodUpdater{span} {};


const std::string HourPeriodUpdater::name() const {
    std::ostringstream os;
    os << std::setw(4) << std::setfill(0) << span_ << "-"
       << "hour";
    return os.str();
}

eckit::DateTime HourPeriodUpdater::computeWinStartTime(const eckit::DateTime& currentTime) {
    return eckit::DateTime{currentTime.date(), eckit::Time{currentTime.time().hours(), 0, 0}};
};

eckit::DateTime HourPeriodUpdater::updateWinEndTime(const eckit::DateTime& startPoint) {
    eckit::DateTime tmp = startPoint + static_cast<eckit::Second>(3600 * span_);
    return eckit::DateTime{tmp.date(), eckit::Time{tmp.time().hours(), 0, 0}};
};

// -------------------------------------------------------------------------------------------------------------------


DayPeriodUpdater::DayPeriodUpdater(long span) : PeriodUpdater{span} {};


const std::string DayPeriodUpdater::name() const {
    std::ostringstream os;
    os << std::setw(4) << std::setfill(0) << span_ << "-"
       << "day";
    return os.str();
}

eckit::DateTime DayPeriodUpdater::computeWinStartTime(const eckit::DateTime& currentTime) {
    return eckit::DateTime{currentTime.date(), eckit::Time{0}};
};

eckit::DateTime DayPeriodUpdater::updateWinEndTime(const eckit::DateTime& startPoint) {
    eckit::DateTime tmp = startPoint + static_cast<eckit::Second>(3600 * 24 * span_);
    return eckit::DateTime{tmp.date(), eckit::Time{0}};
};

// -------------------------------------------------------------------------------------------------------------------


MonthPeriodUpdater::MonthPeriodUpdater(long span) : PeriodUpdater{span} {};


const std::string MonthPeriodUpdater::name() const {
    std::ostringstream os;
    os << std::setw(4) << std::setfill(0) << span_ << "-"
       << "month";
    return os.str();
}

eckit::DateTime MonthPeriodUpdater::computeWinStartTime(const eckit::DateTime& currentTime) {
    auto year = currentTime.date().year();
    auto month = currentTime.date().month();
    return eckit::DateTime{eckit::Date{year, month, 1}, eckit::Time{0}};
};

eckit::DateTime MonthPeriodUpdater::updateWinEndTime(const eckit::DateTime& startPoint) {
    auto totalSpan = startPoint.date().month() + span_ - 1;
    auto endYear = startPoint.date().year() + totalSpan / 12;
    auto endMonth = totalSpan % 12 + 1;
    return eckit::DateTime{eckit::Date{endYear, endMonth, 1}, eckit::Time{0}};
};
}  // namespace multio::action