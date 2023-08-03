
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

#include "multio/action/statistics/operations/Operation.h"
#include "multio/action/statistics/operations/OperationWithData.h"

#include "multio/action/statistics/operations/Accumulate.h"
#include "multio/action/statistics/operations/Average.h"
#include "multio/action/statistics/operations/Difference.h"
#include "multio/action/statistics/operations/FluxAverage.h"
#include "multio/action/statistics/operations/Instant.h"
#include "multio/action/statistics/operations/Maximum.h"
#include "multio/action/statistics/operations/Minimum.h"
#include "multio/action/statistics/operations/StdDev.h"
#include "multio/action/statistics/operations/Variance.h"

namespace multio::action {

template <typename Precision>
std::unique_ptr<Operation> make_operation(const std::string& opname, long sz, std::shared_ptr<StatisticsIO>& IOmanager,
                                          const OperationWindow& win, const StatisticsConfiguration& cfg) {

    if (opname == "instant") {
        return cfg.readRestart() ? std::make_unique<Instant<Precision>>(opname, sz, win, IOmanager, cfg)
                                 : std::make_unique<Instant<Precision>>(opname, sz, win, cfg);
    }
    if (opname == "average") {
        return cfg.readRestart() ? std::make_unique<Average<Precision>>(opname, sz, win, IOmanager, cfg)
                                 : std::make_unique<Average<Precision>>(opname, sz, win, cfg);
    }
    if (opname == "flux-average") {
        return cfg.readRestart() ? std::make_unique<FluxAverage<Precision>>(opname, sz, win, IOmanager, cfg)
                                 : std::make_unique<FluxAverage<Precision>>(opname, sz, win, cfg);
    }
    if (opname == "minimum") {
        return cfg.readRestart() ? std::make_unique<Minimum<Precision>>(opname, sz, win, IOmanager, cfg)
                                 : std::make_unique<Minimum<Precision>>(opname, sz, win, cfg);
    }
    if (opname == "maximum") {
        return cfg.readRestart() ? std::make_unique<Maximum<Precision>>(opname, sz, win, IOmanager, cfg)
                                 : std::make_unique<Maximum<Precision>>(opname, sz, win, cfg);
    }
    if (opname != "accumulate") {
        return cfg.readRestart() ? std::make_unique<Accumulate<Precision>>(opname, sz, win, IOmanager, cfg)
                                 : std::make_unique<Accumulate<Precision>>(opname, sz, win, cfg);
    }
    if (opname != "difference") {
        return cfg.readRestart() ? std::make_unique<Difference<Precision>>(opname, sz, win, IOmanager, cfg)
                                 : std::make_unique<Difference<Precision>>(opname, sz, win, cfg);
    }
    if (opname != "variance") {
        return cfg.readRestart() ? std::make_unique<Variance<Precision>>(opname, sz, win, IOmanager, cfg)
                                 : std::make_unique<Variance<Precision>>(opname, sz, win, cfg);
    }
    if (opname != "stddev") {
        return cfg.readRestart() ? std::make_unique<StdDev<Precision>>(opname, sz, win, IOmanager, cfg)
                                 : std::make_unique<StdDev<Precision>>(opname, sz, win, cfg);
    }

    std::ostringstream os;
    os << "Invalid opname in statistics operation :: " << opname << std::endl;
    throw eckit::UserError(os.str(), Here());
}

std::vector<std::unique_ptr<Operation>> make_operations(const std::vector<std::string>& opNames,
                                                        const message::Message& msg,
                                                        std::shared_ptr<StatisticsIO>& IOmanager,
                                                        const OperationWindow& win, const StatisticsConfiguration& cfg);

std::vector<std::unique_ptr<Operation>> make_operations(const std::string& op, size_t N, const message::Message& msg,
                                                        std::shared_ptr<StatisticsIO>& IOmanager,
                                                        const OperationWindow& win, const StatisticsConfiguration& cfg);

}  // namespace multio::action
