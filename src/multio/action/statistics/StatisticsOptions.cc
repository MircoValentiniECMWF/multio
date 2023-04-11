#include "StatisticsOptions.h"

#include "eckit/exception/Exceptions.h"

namespace multio {
namespace action {

StatisticsOptions::StatisticsOptions(const eckit::LocalConfiguration& confCtx) :
    useDateTime_{false}, stepFreq_{1}, timeStep_{3600}, startDate_{0}, startTime_{0},restart_{false},step_{0},solveSendInitStep_{false} {

    if (!confCtx.has("options")) {
        return;
    }

    const auto& opt = confCtx.getSubConfiguration("options");

    // Overwrite defaults
    useDateTime_ = opt.getBool("use-current-time", false);
    stepFreq_ = opt.getLong("step-frequency", 1L);
    timeStep_ = opt.getLong("time-step", 3600L);
    restart_ = opt.getBool( "restart", false );
    solveSendInitStep_ = opt.getBool("initial-condition-present", false);

    return;
};

StatisticsOptions::StatisticsOptions(const StatisticsOptions& opt, const message::Message& msg) :
    useDateTime_{opt.useDateTime()},
    stepFreq_{opt.stepFreq()},
    timeStep_{opt.timeStep()},
    startDate_{0},
    startTime_{0},
    restart_{opt.restart()},
    step_{0},
    solveSendInitStep_{opt.solver_send_initial_condition()} {

    if (useDateTime() && msg.metadata().has("time")) {
        startTime_ = msg.metadata().getLong("time");
    }
    else if (!useDateTime() && msg.metadata().has("startTime")) {
        startTime_ = msg.metadata().getLong("startTime");
    }
    else {
        throw eckit::SeriousBug{"Unable to find start time", Here()};
    }

    if (useDateTime() && msg.metadata().has("date")) {
        startDate_ = msg.metadata().getLong("date");
    }
    else if (!useDateTime() && msg.metadata().has("startDate")) {
        startDate_ = msg.metadata().getLong("startDate");
    }
    else {
        throw eckit::SeriousBug{"Unable to find start date", Here()};
    }

    // Step is here in case we need some hacks
    if ( !msg.metadata().has("step") ){
        throw eckit::SeriousBug{"Step metadata not present", Here()};
    }
    step_ = msg.metadata().getLong("step");

    timeStep_ = msg.metadata().getLong("timeStep", timeStep_);
    stepFreq_ = msg.metadata().getLong("step-frequency", stepFreq_);

    return;
};

bool StatisticsOptions::restart() const {
    return restart_;
};

bool StatisticsOptions::useDateTime() const {
    return useDateTime_;
};

long StatisticsOptions::stepFreq() const {
    return stepFreq_;
};
long StatisticsOptions::timeStep() const {
    return timeStep_;
};

long StatisticsOptions::startDate() const {
    return startDate_;
}

long StatisticsOptions::startTime() const {
    return startTime_;
}

long StatisticsOptions::step() const {
    return step_;
}

bool StatisticsOptions::solver_send_initial_condition() const{
    return solveSendInitStep_;
}

}  // namespace action
}  // namespace multio
