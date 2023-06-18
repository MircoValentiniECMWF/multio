/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Tiago Quintino

/// @date Jan 2019

#pragma once

#include <tuple>

#include "PeriodUpdater.h"
#include "StatisticsConfiguration.h"
#include "StatisticsIO.h"
#include "multio/action/ChainedAction.h"

#include "TemporalStatistics.h"

namespace eckit {
class Configuration;
}

namespace multio::action {


template <typename T, template <typename Precision> typename Operation, typename Updater>
using precisionMap = std::map<std::string, std::unique_ptr<TemporalStatistics<T, Operation, Updater>>>;


template <template <typename Precision> typename Operation, typename Updater>
using multiMap = std::tuple<precisionMap<float, Operation, Updater>, precisionMap<double, Operation, Updater>>;

template <template <typename Precision> typename Operation, typename Updater>
class Statistics : public ChainedAction {
public:
    explicit Statistics(const ComponentConfiguration& compConf);
    void executeImpl(message::Message msg) override;

private:
    const StatisticsConfiguration cfg_;
    std::shared_ptr<StatisticsIO> IOmanager_;

    void DumpRestart();
    std::string generateKey(const message::Message& msg) const;
    void print(std::ostream& os) const override;


    message::Metadata outputMetadata(const MovingWindow& win, const std::string timeUnit,
                                     const message::Metadata& inputMetadata, const StatisticsConfiguration& opt,
                                     const std::string& key) const;

    template <typename Precision>
    bool update(const message::Message& msg, StatisticsConfiguration& cfg);

    template <typename Precision>
    message::Message compute(message::Message&& msg, StatisticsConfiguration& cfg);

    multiMap<Operation, Updater> maps_;
};

}  // namespace multio::action
