#pragma once

#include "StatisticsConfiguration.h"
#include "eckit/types/DateTime.h"
#include "multio/message/Message.h"

namespace multio::action {

class PeriodUpdater {
public:
    PeriodUpdater(long span);
    virtual const std::string name() const = 0;
    eckit::DateTime updatePeriodStart(const message::Message& msg, StatisticsConfiguration& cfg);
    eckit::DateTime updatePeriodEnd(const message::Message& msg, StatisticsConfiguration& cfg);
    MovingWindow initPeriod(const message::Message& msg, const StatisticsConfiguration& cfg);

private:
    eckit::DateTime computeWinCreationTime(const eckit::DateTime& currentTime);
    virtual eckit::DateTime computeWinStartTime(const eckit::DateTime& currentTime) = 0;
    virtual eckit::DateTime computeWinEndTime(const eckit::DateTime& startPoint) = 0;
    const long span_;
}


// -------------------------------------------------------------------------------------------------------------------


class HourPeriodUpdater {
public:
    HourPeriodUpdater(long span);
    const std::string name() const;

private:
    eckit::DateTime computeWinStartTime(const eckit::DateTime& currentTime);
    eckit::DateTime computeWinEndTime(const eckit::DateTime& startPoint);
}


// -------------------------------------------------------------------------------------------------------------------


class DayPeriodUpdater {
public:
    DayPeriodUpdater(long span);
    const std::string name() const;

private:
    eckit::DateTime computeWinStartTime(const eckit::DateTime& currentTime);
    eckit::DateTime computeWinEndTime(const eckit::DateTime& startPoint);
}


// -------------------------------------------------------------------------------------------------------------------


class MonthPeriodUpdater {
public:
    MonthPeriodUpdater(long span);
    const std::string name() const;

private:
    eckit::DateTime computeWinStartTime(const eckit::DateTime& currentTime);
    eckit::DateTime computeWinEndTime(const eckit::DateTime& startPoint);
}


// -------------------------------------------------------------------------------------------------------------------


std::unique_ptr<PeriodUpdater>
make_period_updater(const std::string& periodKind, long span) {
    switch (periodKind) {
        case "h": {
            return make_unique<HourPeriod>(span);
        }
        case "d": {
            return make_unique<DayPeriod>(span);
        }
        case "m": {
            return make_unique<MonthPeriod>(span);
        }
        default: {
            throw eckit::SeriousBug("Unknown period kind", Here());
        }
    }
}

}  // namespace multio::action