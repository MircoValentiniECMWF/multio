#include "TemporalStatistics.h"

#include <algorithm>
#include <cstring>
#include <fstream>
#include <iostream>

#include "eckit/exception/Exceptions.h"
#include "multio/LibMultio.h"
#include "multio/util/PrecisionTag.h"

#include "windows/DailyStatistics.h"
#include "windows/HourlyStatistics.h"
#include "windows/MonthlyStatistics.h"

namespace multio::action {

std::unique_ptr<TemporalStatistics> TemporalStatistics::build(const std::string& unit, long span,
                                                              const std::vector<std::string>& operations,
                                                              const message::Message& msg, StatisticsIO& IOmanager,
                                                              const StatisticsConfiguration& cfg) {

    if (unit == "month") {
        return std::make_unique<MonthlyStatistics>(operations, span, msg, IOmanager, cfg);
    }

    if (unit == "day") {
        return std::make_unique<DailyStatistics>(operations, span, msg, IOmanager, cfg);
    }

    if (unit == "hour") {
        return std::make_unique<HourlyStatistics>(operations, span, msg, IOmanager, cfg);
    }

    throw eckit::SeriousBug{"Temporal statistics for base period " + unit + " is not defined"};
}


TemporalStatistics::TemporalStatistics(const std::vector<std::string>& operations, const DateTimePeriod& period,
                                       const message::Message& msg, StatisticsIO& IOmanager,
                                       const StatisticsConfiguration& cfg, long span) :
    periodUpdater{make_period_updater(unit, span)},
    window_{periodUpdater_->initPeriod(msg, cfg)},
    statistics_{make_operations(operations, msg, IOmanager, window_, cfg)} {}


void TemporalStatistics::dump(StatisticsIO& IOmanager, const StatisticsConfiguration& cfg) const {
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " *** Dump restart file" << std::endl;
    // TODO: set file name and other file options
    window_.dump(IOmanager);
    for (auto& stat : statistics_) {
        stat->dump(IOmanager);
    }
    return;
}

void TemporalStatistics::updateData(message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(multio::LibMultio) << cfg.logPrefix() << " *** Update ";
    window_.updateData(currentDateTime(msg, cfg));
    for (auto& stat : statistics_) {
        stat->updateData(msg.payload().data(), msg.size());
    }
    return;
}

void TemporalStatistics::updateWindow(const message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(::multio::LibMultio) << cfg.logPrefix() << " *** Reset window " << std::endl;
    eckit::DateTime startPoint = periodUpdater_->updatePeriodStart(msg, cfg);
    eckit::DateTime endPoint = periodUpdater_->updatePeriodEnd(msg, cfg);
    window_.updateWindow(startPoint, endPoint);
    for (auto& stat : statistics_) {
        stat->reset(msg.payload().data(), msg.size());
    }
    return;
}

bool TemporalStatistics::isEndOfWindow(message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(::multio::LibMultio) << cfg.logPrefix() << " *** Check end of window " << std::endl;
    return !window_.isWithin(nextDateTime(msg, cfg));
}

bool TemporalStatistics::isBeginOfWindow(message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(::multio::LibMultio) << cfg.logPrefix() << " *** Check begin of window " << std::endl;
    return !window_.isWithin(currentDateTime(msg, cfg));
}

const MovingWindow& TemporalStatistics::win_() const {
    return window_;
}

void print(std::ostream& os) const {
    os << "Temporal Statistics";
}

}  // namespace multio::action
