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


#include "StatisticsConfiguration.h"
#include "StatisticsIO.h"
#include "multio/action/ChainedAction.h"

namespace eckit {
class Configuration;
}

namespace multio::action {

class TemporalStatistics;

class Statistics : public ChainedAction {
public:
    explicit Statistics(const ConfigurationContext& confCtx);
    void executeImpl(message::Message msg) override;
    message::Metadata outputMetadata(const message::Metadata& inputMetadata, const StatisticsConfiguration& opt,
                                     const std::string& key) const;

private:
    void DumpRestart() const;
    std::string generateKey(const message::Message& msg) const;
    void print(std::ostream& os) const override;
    const std::string timeUnit_;
    const long timeSpan_;
    const std::vector<std::string> operations_;
    const StatisticsConfiguration cfg_;

    std::map<std::string, std::unique_ptr<TemporalStatistics>> fieldStats_;
};

}  // namespace multio::action
