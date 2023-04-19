#include "StatisticsOptions.h"

#include <limits.h>
#include <unistd.h>
#include <iomanip>


#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"

#include "multio/util/Substitution.h"

namespace multio {
namespace action {


StatisticsOptions::StatisticsOptions(const eckit::LocalConfiguration& confCtx) :
    useDateTime_{false},
    stepFreq_{1},
    timeStep_{3600},
    startDate_{0},
    startTime_{0},
    restart_{false},
    step_{-1},
    solverSendInitStep_{false},
    missingValue_{9999.0},
    missingValueTolerance_{1.0E-12},
    haveMissingValue_{false},
    restartPath_{"."},
    restartPrefix_{"StatisticsRestartFile"},
    logPrefix_{"Plan"} {

    if (!confCtx.has("options")) {
        return;
    }

    const auto& opt = confCtx.getSubConfiguration("options");

    // TODO:: remove boilerplate code (same code in ConfigurationContext.cc)
    auto env = [](std::string_view replace) {
        std::string lookUpKey{replace};
        char* env = ::getenv(lookUpKey.c_str());
        if (env) {
            return eckit::Optional<std::string>{env};
        }
        else {
            return eckit::Optional<std::string>{};
        }
    };

    // Overwrite defaults
    useDateTime_ = opt.getBool("use-current-time", false);
    stepFreq_ = opt.getLong("step-frequency", 1L);
    timeStep_ = opt.getLong("time-step", 3600L);
    solverSendInitStep_ = opt.getBool("initial-condition-present", false);
    eckit::Optional<bool> r = util::parseBool(opt, "restart", false);
    if (r) {
        restart_ = *r;
    }
    else {
        throw eckit::SeriousBug{"Unable to restart value", Here()};
    }
    // TODO: Add functionality to automatically create restart path if it not exists
    // (same improvement can be done in sink). Feature already present in eckit::PathName
    if (opt.has("restart-path")) {
        restartPath_ = util::replaceCurly(opt.getString("restart-path", "."), env);
        eckit::PathName path{restartPath_};
        if (!path.exists() || !path.isDir()) {
            throw eckit::UserError{"restart path not exist", Here()};
        }
    }
    missingValueTolerance_ = opt.getDouble("missing-value-tolerance", 1.0E-12);
    restartPrefix_ = util::replaceCurly(opt.getString("restart-prefix", "StatisticsDump"), env);
    logPrefix_ = util::replaceCurly(opt.getString("log-prefix", "Plan"), env);

    return;
};

StatisticsOptions::StatisticsOptions(const StatisticsOptions& opt, const message::Message& msg) :
    useDateTime_{opt.useDateTime()},
    stepFreq_{opt.stepFreq()},
    timeStep_{opt.timeStep()},
    startDate_{0},
    startTime_{0},
    restart_{opt.restart()},
    step_{-1},
    solverSendInitStep_{opt.solver_send_initial_condition()},
    missingValue_{9999.0},
    missingValueTolerance_{1.0E-12},
    haveMissingValue_{false},
    restartPath_{opt.restartPath()},
    restartPrefix_{opt.restartPrefix()},
    logPrefix_{""} {

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
    if (!msg.metadata().has("step")) {
        throw eckit::SeriousBug{"Step metadata not present", Here()};
    }
    step_ = msg.metadata().getLong("step");

    // step and frequency
    timeStep_ = msg.metadata().getLong("timeStep", timeStep_);
    stepFreq_ = msg.metadata().getLong("step-frequency", stepFreq_);

    // Logging prefix
    // TODO: can be skipped if MULTIO_DEBUG is 0
    std::ostringstream os;
    if (opt.logPrefix() != "Plan") {
        os << "(prefix=" << opt.logPrefix();
    }
    else if (opt.restartPrefix() != "StatisticsRestartFile") {
        os << "(prefix=" << opt.restartPrefix();
    }
    os << ", step=" << std::left << std::setw(6) << step_;
    if (msg.metadata().has("param")) {
        os << ", param=" << std::left << std::setw(10) << msg.metadata().getString("param");
    }
    else if (msg.metadata().has("paramId")) {
        os << ", param=" << std::left << std::setw(10) << msg.metadata().getString("paramId");
    }
    else {
        throw eckit::SeriousBug{"param/paramId metadata not present", Here()};
    }
    if (msg.metadata().has("level")) {
        os << ", level=" << std::left << std::setw(4) << msg.metadata().getLong("level");
    }
    else if (msg.metadata().has("levelist")) {
        os << ", level=" << std::left << std::setw(4) << msg.metadata().getLong("levelist");
    }
    if (msg.metadata().has("levtype")) {
        os << ", level-type=" << std::left << std::setw(5) << msg.metadata().getString("levtype");
    }
    os << ", pid=" << std::left << std::setw(10) << ::getpid();

    {
        char hostname[255];
        gethostname(hostname, 255);
        os << ", hostname=" << std::string{hostname} << ") ";
    }
    logPrefix_ = os.str();

    // Handle missing values
    if (msg.metadata().has("missingValue")) {
        haveMissingValue_ = true;
        missingValue_ = msg.metadata().getDouble("missingValue");
    }

    return;
};

bool StatisticsOptions::restart() const {
    return ((step_ == 0 && solverSendInitStep_) || (step_ == 1 && solverSendInitStep_)) ? false : restart_;
};

const std::string& StatisticsOptions::restartPath() const {
    return restartPath_;
};

const std::string& StatisticsOptions::restartPrefix() const {
    return restartPrefix_;
};

const std::string& StatisticsOptions::logPrefix() const {
    return logPrefix_;
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

bool StatisticsOptions::solver_send_initial_condition() const {
    return solverSendInitStep_;
}

bool StatisticsOptions::haveMissingValue() const {
    return haveMissingValue_;
};


double StatisticsOptions::missingValue() const {
    return missingValue_;
};


double StatisticsOptions::missingValueTolerance() const {
    return missingValueTolerance_;
};

}  // namespace action
}  // namespace multio
