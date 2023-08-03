#include "TemporalStatistics.h"

#include <iostream>

#include "eckit/types/DateTime.h"

#include "multio/LibMultio.h"

#include "TimeUtils.h"

namespace multio::action {


TemporalStatistics::TemporalStatistics(const std::shared_ptr<PeriodUpdater>& periodUpdater,
                                       const std::vector<std::string>& operations, SynopticMatchers& matchers,
                                       const message::Message& msg, std::shared_ptr<StatisticsIO>& IOmanager,
                                       const StatisticsConfiguration& cfg) :
    periodUpdater_{periodUpdater},
    window_{periodUpdater_->initPeriod(msg, IOmanager, cfg)},
    collections_{make_collections(operations, matchers, msg, IOmanager, window_, cfg)} {}


void TemporalStatistics::dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) const {
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " *** Dump restart files" << std::endl;
    window_.dump(IOmanager, cfg);
    for (auto& coll : collections_) {
        coll->dump(IOmanager, cfg);
    }
    IOmanager->flush();
    return;
}

void TemporalStatistics::updateData(message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(multio::LibMultio) << cfg.logPrefix() << " *** Update Data" << std::endl;
    window_.updateData(currentDateTime(msg, cfg));
    for (auto& coll : collections_) {
        coll->updateData(msg, cfg);
    }
    return;
}

void TemporalStatistics::updateWindow(const message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(::multio::LibMultio) << cfg.logPrefix() << " *** Update Window " << std::endl;
    window_.updateWindow(periodUpdater_->updatePeriodStart(msg, cfg), periodUpdater_->updatePeriodEnd(msg, cfg));
    for (auto& coll : collections_) {
        coll->resetWindow(msg, cfg);
    }
    return;
}

bool TemporalStatistics::isEndOfWindow(message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(::multio::LibMultio) << cfg.logPrefix() << " *** Check end of Window " << std::endl;
    return !window_.isWithin(nextDateTime(msg, cfg));
}

const OperationWindow& TemporalStatistics::cwin() const {
    return window_;
}

OperationWindow& TemporalStatistics::win() {
    return window_;
}

void TemporalStatistics::print(std::ostream& os) const {
    os << "Temporal Statistics";
}

}  // namespace multio::action
