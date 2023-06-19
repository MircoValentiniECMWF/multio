/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include "Statistics.h"

#include <algorithm>
#include <unordered_map>

#include "eckit/exception/Exceptions.h"
#include "multio/LibMultio.h"
#include "multio/message/Message.h"
#include "multio/util/ScopedTimer.h"

namespace multio::action {

namespace {
template <size_t I = 0, typename... Ts>
typename std::enable_if<I == sizeof...(Ts), void>::type dumpMultiMap(std::tuple<Ts...>& tup,
                                                                     const StatisticsConfiguration& cfg,
                                                                     std::shared_ptr<StatisticsIO>& IOmanager) {
    return;
}

template <size_t I = 0, typename... Ts>
typename std::enable_if<(I < sizeof...(Ts)), void>::type dumpMultiMap(std::tuple<Ts...>& tup,
                                                                      const StatisticsConfiguration& cfg,
                                                                      std::shared_ptr<StatisticsIO>& IOmanager) {
    // TODO: improve the removing of old files
    for (auto it = std::get<I>(tup).begin(); it != std::get<I>(tup).end(); it++) {
        LOG_DEBUG_LIB(LibMultio) << "Restart for field with key :: " << it->first << ", "
                                 << it->second->cwin().currPointInSteps() << std::endl;
        IOmanager->setCurrStep(it->second->cwin().currPointInSteps());
        IOmanager->setPrevStep(it->second->cwin().lastFlushInSteps());
        IOmanager->setKey(it->first);
        it->second->dump(IOmanager, cfg);
        it->second->win().updateFlush();
    }
    dumpMultiMap<I + 1>(tup, cfg, IOmanager);
}

}  // namespace

template <template <typename Precision> typename Operation, typename Updater>
Statistics<Operation, Updater>::Statistics(const ComponentConfiguration& compConf) :
    ChainedAction{compConf},
    cfg_{compConf},
    IOmanager_{StatisticsIOFactory::instance().build(cfg_.restartLib(), cfg_.restartPath(), cfg_.restartPrefix())} {}


template <template <typename Precision> typename Operation, typename Updater>
void Statistics<Operation, Updater>::DumpRestart() {
    if (cfg_.writeRestart()) {
        IOmanager_->reset();
        dumpMultiMap(maps_, cfg_, IOmanager_);
    }
}


template <template <typename Precision> typename Operation, typename Updater>
std::string Statistics<Operation, Updater>::generateKey(const message::Message& msg) const {
    std::ostringstream os;
    os << msg.metadata().getString("param", "") << "-" << msg.metadata().getString("paramId", "") << "-"
       << msg.metadata().getLong("level", 0) << "-" << msg.metadata().getLong("levelist", 0) << "-"
       << msg.metadata().getString("levtype", "unknown") << "-" << msg.metadata().getString("gridType", "unknown")
       << "-" << msg.metadata().getString("precision", "unknown") << "-"
       << std::to_string(std::hash<std::string>{}(msg.source()));
    LOG_DEBUG_LIB(LibMultio) << "Generating key for the field :: " << os.str() << std::endl;
    return os.str();
}


template <template <typename Precision> typename Operation, typename Updater>
message::Metadata Statistics<Operation, Updater>::outputMetadata(const MovingWindow& win, const std::string timeUnit,
                                                                 const message::Metadata& inputMetadata,
                                                                 const StatisticsConfiguration& cfg,
                                                                 const std::string& key) const {
    if (win.endPointInSeconds() % 3600 != 0L) {
        std::ostringstream os;
        os << "Step in seconds needs to be a multiple of 3600 :: " << win.endPointInSeconds() << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    auto md = inputMetadata;
    md.set("timeUnit", timeUnit);
    md.set("startDate", win.epochPoint().date().yyyymmdd());
    md.set("startTime", win.epochPoint().time().hhmmss());
    md.set("timeSpanInHours", win.timeSpanInHours());
    md.set("stepRange", win.stepRange());
    md.set("previousDate", win.creationPoint().date().yyyymmdd());
    md.set("previousTime", win.creationPoint().time().hhmmss());
    md.set("currentDate", win.endPoint().date().yyyymmdd());
    md.set("currentTime", win.endPoint().time().hhmmss());
    md.set("stepInHours", win.endPointInHours());
    md.set("stepRangeInHours", win.stepRangeInHours());
    return md;
}


template <template <typename Precision> typename Operation, typename Updater>
template <typename T>
bool Statistics<Operation, Updater>::update(const message::Message& msg, StatisticsConfiguration& cfg) {


    std::string key = generateKey(msg);
    precisionMap<T, Operation, Updater>& PrecisionMap = std::get<precisionMap<T, Operation, Updater>>(maps_);

    IOmanager_->reset();
    IOmanager_->setCurrStep(cfg.restartStep());
    IOmanager_->setKey(key);

    if (PrecisionMap.find(key) == PrecisionMap.end()) {
        PrecisionMap[key] = std::make_unique<TemporalStatistics<T, Operation, Updater>>(msg, IOmanager_, cfg);
        if (cfg.solver_send_initial_condition()) {
            return false;
        }
    }

    auto& Ts = PrecisionMap.at(key);
    Ts->updateData(msg, cfg);

    return Ts->isEndOfWindow(msg, cfg);
}


template <template <typename Precision> typename Operation, typename Updater>
template <typename T>
message::Message Statistics<Operation, Updater>::compute(message::Message&& msg, StatisticsConfiguration& cfg) {

    std::string key = generateKey(msg);
    precisionMap<T, Operation, Updater>& PrecisionMap = std::get<precisionMap<T, Operation, Updater>>(maps_);
    auto& Ts = PrecisionMap.at(key);

    auto md = outputMetadata(Ts->cwin(), Ts->timeUnit(), msg.metadata(), cfg, key);
    md.set("operation", Ts->operation(cfg));
    Ts->compute(msg.payload(), cfg);
    Ts->updateWindow(msg, cfg);

    return {
        message::Message::Header{message::Message::Tag::Field, msg.source(), msg.destination(), message::Metadata{md}},
        std::move(msg.payload())};
}


template <template <typename Precision> typename Operation, typename Updater>
void Statistics<Operation, Updater>::executeImpl(message::Message msg) {

    util::ScopedTiming timing{statistics_.localTimer_, statistics_.actionTiming_};

    if (msg.tag() == message::Message::Tag::Flush) {
        DumpRestart();
        executeNext(msg);
        return;
    }

    if (msg.tag() != message::Message::Tag::Field) {
        executeNext(msg);
        return;
    }

    StatisticsConfiguration cfg{cfg_, msg};


    bool endOfWindow = util::dispatchPrecisionTag(msg.precision(), [&](auto pt) -> bool {
        using Precision = typename decltype(pt)::type;
        return update<Precision>(msg, cfg);
    });

    if (endOfWindow) {
        executeNext(util::dispatchPrecisionTag(msg.precision(), [&](auto pt) -> message::Message {
            using Precision = typename decltype(pt)::type;
            return compute<Precision>(std::move(msg), cfg);
        }));
    }
}


template <template <typename Precision> typename Operation, typename Updater>
void Statistics<Operation, Updater>::print(std::ostream& os) const {
    os << "Statistics";
    return;
}


static ActionBuilder<Statistics<Average, MonthPeriodUpdater>> MonthlyAverageBuilder("monthly-average");
static ActionBuilder<Statistics<Average, DayPeriodUpdater>> DailyAverageBuilder("daily-average");
static ActionBuilder<Statistics<Average, HourPeriodUpdater>> HourlyAverageBuilder("hourly-average");

static ActionBuilder<Statistics<FluxAverage, MonthPeriodUpdater>> MonthlyFluxAverageBuilder("monthly-flux-average");
static ActionBuilder<Statistics<FluxAverage, DayPeriodUpdater>> DailyFluxAverageBuilder("daily-flux-average");
static ActionBuilder<Statistics<FluxAverage, HourPeriodUpdater>> HourlyFluxAverageBuilder("hourly-flux-average");

static ActionBuilder<Statistics<Accumulate, MonthPeriodUpdater>> MonthlyAccumulateBuilder("monthly-accumulate");
static ActionBuilder<Statistics<Accumulate, DayPeriodUpdater>> DailyAccumulateBuilder("daily-accumulate");
static ActionBuilder<Statistics<Accumulate, HourPeriodUpdater>> HourlyAccumulateBuilder("hourly-accumulate");

static ActionBuilder<Statistics<Instant, MonthPeriodUpdater>> MonthlyInstantBuilder("monthly-instant");
static ActionBuilder<Statistics<Instant, DayPeriodUpdater>> DailyInstantBuilder("daily-instant");
static ActionBuilder<Statistics<Instant, HourPeriodUpdater>> HourlyInstantBuilder("hourly-instant");

static ActionBuilder<Statistics<Maximum, MonthPeriodUpdater>> MonthlyMaximumBuilder("monthly-maximum");
static ActionBuilder<Statistics<Maximum, DayPeriodUpdater>> DailyMaximumBuilder("daily-maximum");
static ActionBuilder<Statistics<Maximum, HourPeriodUpdater>> HourlyMaximumBuilder("hourly-maximum");

static ActionBuilder<Statistics<Minimum, MonthPeriodUpdater>> MonthlyMinimumBuilder("monthly-minimum");
static ActionBuilder<Statistics<Minimum, DayPeriodUpdater>> DailyMinimumBuilder("daily-minimum");
static ActionBuilder<Statistics<Minimum, HourPeriodUpdater>> HourlyMinimumBuilder("hourly-minimum");

static ActionBuilder<Statistics<Difference, MonthPeriodUpdater>> MonthlyDifferenceBuilder("monthly-difference");
static ActionBuilder<Statistics<Difference, DayPeriodUpdater>> DailyDifferenceBuilder("daily-difference");
static ActionBuilder<Statistics<Difference, HourPeriodUpdater>> HourlyDifferenceBuilder("hourly-difference");

static ActionBuilder<Statistics<StdDev, MonthPeriodUpdater>> MonthlyStdDevBuilder("monthly-stddev");
static ActionBuilder<Statistics<StdDev, DayPeriodUpdater>> DailyStdDevBuilder("daily-stddev");
static ActionBuilder<Statistics<StdDev, HourPeriodUpdater>> HourlyStdDevBuilder("hourly-stddev");

}  // namespace multio::action
