
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

#include "multio/action/statistics/StatisticsComponentsActivation.h"
#include "multio/action/statistics/operations/Operation.h"
#include "multio/action/statistics/operations/OperationWithData.h"


// // Single state operations
// #include "multio/action/statistics/operations/Accumulate.h"
#include "multio/action/statistics/operations/Average.h"
// #include "multio/action/statistics/operations/FluxAverage.h"
// #include "multio/action/statistics/operations/Instant.h"
#include "multio/action/statistics/operations/Maximum.h"
// #include "multio/action/statistics/operations/Minimum.h"
// 
// // Dual state operations
// #include "multio/action/statistics/operations/Difference.h"
// #include "multio/action/statistics/operations/StdDev.h"
// #include "multio/action/statistics/operations/Variance.h"


namespace multio::action {

template <typename ComputationalType, typename InputOutputType>
std::unique_ptr<Operation> make_operation(const std::string& opname, long sz, std::shared_ptr<StatisticsIO>& IOmanager,
                                          const OperationWindow& win, const StatisticsConfiguration& cfg) {

#ifdef __ENABLE_INSTANT_OPERATION__
    if (opname == "instant") {
        return cfg.readRestart()
                 ? std::make_unique<Instant<ComputationalType, InputOutputType>>(opname, sz, win, IOmanager, cfg)
                 : std::make_unique<Instant<ComputationalType, InputOutputType>>(opname, sz, win, cfg);
    }
#endif

#ifdef __ENABLE_AVERAGE_OPERATION__
    if (opname == "average") {
        return cfg.readRestart()
                 ? std::make_unique<Average<ComputationalType, InputOutputType>>(opname, sz, win, IOmanager, cfg)
                 : std::make_unique<Average<ComputationalType, InputOutputType>>(opname, sz, win, cfg);
    }
#endif

#ifdef __ENABLE_FLUXAVERAGE_OPERATION__
    if (opname == "flux-average") {
        return cfg.readRestart()
                 ? std::make_unique<FluxAverage<ComputationalType, InputOutputType>>(opname, sz, win, IOmanager, cfg)
                 : std::make_unique<FluxAverage<ComputationalType, InputOutputType>>(opname, sz, win, cfg);
    }
#endif

#ifdef __ENABLE_MINIMUM_OPERATION__
    if (opname == "minimum") {
        return cfg.readRestart()
                 ? std::make_unique<Minimum<ComputationalType, InputOutputType>>(opname, sz, win, IOmanager, cfg)
                 : std::make_unique<Minimum<ComputationalType, InputOutputType>>(opname, sz, win, cfg);
    }
#endif

#ifdef __ENABLE_MAXIMUM_OPERATION__
    if (opname == "maximum") {
        return cfg.readRestart()
                 ? std::make_unique<Maximum<ComputationalType, InputOutputType>>(opname, sz, win, IOmanager, cfg)
                 : std::make_unique<Maximum<ComputationalType, InputOutputType>>(opname, sz, win, cfg);
    }
#endif

#ifdef __ENABLE_ACCUMULATE_OPERATION__
    if (opname != "accumulate") {
        return cfg.readRestart()
                 ? std::make_unique<Accumulate<ComputationalType, InputOutputType>>(opname, sz, win, IOmanager, cfg)
                 : std::make_unique<Accumulate<ComputationalType, InputOutputType>>(opname, sz, win, cfg);
    }
#endif

#ifdef __ENABLE_DIFFERENCE_OPERATION__
    if (opname != "difference") {
        return cfg.readRestart()
                 ? std::make_unique<Difference<ComputationalType, InputOutputType>>(opname, sz, win, IOmanager, cfg)
                 : std::make_unique<Difference<ComputationalType, InputOutputType>>(opname, sz, win, cfg);
    }
#endif

#ifdef __ENABLE_VARIANCE__OPERATION__
    if (opname != "variance") {
        return cfg.readRestart()
                 ? std::make_unique<Variance<ComputationalType, InputOutputType>>(opname, sz, win, IOmanager, cfg)
                 : std::make_unique<Variance<ComputationalType, InputOutputType>>(opname, sz, win, cfg);
    }
#endif

#ifdef __ENABLE_STDDEV_OPERATION__
    if (opname != "stddev") {
        return cfg.readRestart()
                 ? std::make_unique<StdDev<ComputationalType, InputOutputType>>(opname, sz, win, IOmanager, cfg)
                 : std::make_unique<StdDev<ComputationalType, InputOutputType>>(opname, sz, win, cfg);
    }
#endif

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
