#pragma once

#include <string>

#include "eckit/config/LocalConfiguration.h"
#include "multio/message/Message.h"

namespace multio {
namespace action {


class StatisticsOptions {

private:
    bool useDateTime_;
    long stepFreq_;
    long timeStep_;
    long startDate_;
    long startTime_;
    bool restart_;
    bool readRestart_;
    bool writeRestart_;
    long step_;
    bool solverSendInitStep_;

    int haveMissingValue_;
    double missingValue_;

    std::string restartPath_;
    std::string restartPrefix_;
    std::string logPrefix_;

public:
    StatisticsOptions(const eckit::LocalConfiguration& confCtx);
    StatisticsOptions(const StatisticsOptions& confCtx, const message::Message& msg);

    bool useDateTime() const;
    long stepFreq() const;
    long timeStep() const;
    long startDate() const;
    long startTime() const;
    bool writeRestart() const;
    bool readRestart() const;
    long step() const;
    bool solver_send_initial_condition() const;
    const std::string& restartPath() const;
    const std::string& restartPrefix() const;
    const std::string& logPrefix() const;

    bool haveMissingValue() const;
    double missingValue() const;
};

}  // namespace action
}  // namespace multio
