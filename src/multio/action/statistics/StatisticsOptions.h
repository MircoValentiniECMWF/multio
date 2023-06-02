#pragma once

#include <string>

#include "eckit/config/LocalConfiguration.h"
#include "multio/message/Message.h"

#include "StatisticsIO.h"

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
    long restartStep_;
    bool solverSendInitStep_;

    int haveMissingValue_;
    double missingValue_;

    std::string restartPath_;
    std::string restartPrefix_;
    std::string logPrefix_;

    mutable StatisticsIO reader_;
    mutable StatisticsIO dumper_;

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
    long restartStep() const;
    bool solver_send_initial_condition() const;
    const std::string& restartPath() const;
    const std::string& restartPrefix() const;
    const std::string& logPrefix() const;

    bool haveMissingValue() const;
    double missingValue() const;

    StatisticsIO& reader(){ return reader_; };
    StatisticsIO& dumper(){ return dumper_; };
    
};

}  // namespace action
}  // namespace multio
