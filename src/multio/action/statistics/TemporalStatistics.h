#pragma once

#include <cmath>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "eckit/exception/Exceptions.h"

#include "multio/LibMultio.h"
#include "multio/message/Message.h"

#include "multio/action/statistics/MovingWindow.h"
#include "multio/action/statistics/StatisticsConfiguration.h"
#include "multio/action/statistics/StatisticsIO.h"

#include "multio/action/statistics/operations/OperationBase.h"
#include "multio/action/statistics/operations/OperationWithData.h"

#include "multio/action/statistics/operations/Accumulate.h"
#include "multio/action/statistics/operations/Average.h"
#include "multio/action/statistics/operations/FluxAverage.h"
#include "multio/action/statistics/operations/Instant.h"
#include "multio/action/statistics/operations/Maximum.h"
#include "multio/action/statistics/operations/Minimum.h"

#include "MovingWindow.h"
#include "PeriodUpdater.h"
#include "StatisticsConfiguration.h"
#include "StatisticsIO.h"
#include "TimeUtils.h"

namespace multio::action {

template <typename Precision, template <typename Precision> typename Operation, typename Updater>
class TemporalStatistics {
public:
    TemporalStatistics(const message::Message& msg, std::shared_ptr<StatisticsIO>& IOmanager,
                       const StatisticsConfiguration& cfg) :
        periodUpdater_{cfg.span()},
        window_{periodUpdater_.initPeriod(msg, IOmanager, cfg)},
        stat_{cfg.readRestart() ? Operation<Precision>(msg.size(), window_, IOmanager, cfg)
                                : Operation<Precision>(msg.size(), window_, cfg)} {
        LOG_DEBUG_LIB(::multio::LibMultio)
            << cfg.logPrefix() << " :: " << *this << " :: Create new statistics " << std::endl;
        stat_.init(msg.payload(), msg.size());
        return;
    };


    bool isEndOfWindow(const message::Message& msg, const StatisticsConfiguration& cfg) {
        LOG_DEBUG_LIB(::multio::LibMultio)
            << cfg.logPrefix() << " :: " << *this << " :: Check end of Window " << std::endl;
        return !window_.isWithin(nextDateTime(msg, cfg));
    };


    void updateData(const message::Message& msg, const StatisticsConfiguration& cfg) {
        LOG_DEBUG_LIB(multio::LibMultio) << cfg.logPrefix() << " :: " << *this << " :: Update Data" << std::endl;
        window_.updateData(currentDateTime(msg, cfg));
        stat_.updateData(msg.payload().data(), msg.size());
        return;
    };


    void updateWindow(const message::Message& msg, const StatisticsConfiguration& cfg) {
        LOG_DEBUG_LIB(::multio::LibMultio) << cfg.logPrefix() << " :: " << *this << " :: Update Window " << std::endl;
        window_.updateWindow(periodUpdater_.updatePeriodStart(msg, cfg), periodUpdater_.updatePeriodEnd(msg, cfg));
        stat_.updateWindow(msg.payload().data(), msg.size());
        return;
    };


    void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) const {
        LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " :: " << *this << " :: Dump restart files" << std::endl;
        IOmanager->setSuffix(periodUpdater_.name());
        window_.dump(IOmanager, cfg);
        stat_.dump(IOmanager, cfg);
        IOmanager->flush();
        return;
    };


    void compute(eckit::Buffer& buf, const StatisticsConfiguration& cfg) {
        LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " :: " << *this << " :: Compute statistics" << std::endl;
        stat_.compute(buf);
        return;
    }


    std::string operation(const StatisticsConfiguration& cfg) const {
        LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " :: " << *this << " :: Return operation type" << std::endl;
        return stat_.operation();
    }


    const MovingWindow& win() const { return window_; };

    void print(std::ostream& os) const {
        os << "TemporalStatistics<" << stat_.operation() << ", " << timeUnit() << ">";
    };

    const std::string timeUnit() const { return periodUpdater_.timeUnit(); };

private:
    Updater periodUpdater_;
    MovingWindow window_;
    Operation<Precision> stat_;

    friend std::ostream& operator<<(std::ostream& os, const TemporalStatistics& a) {
        a.print(os);
        return os;
    }
};

}  // namespace multio::action
