
#include "Operations.h"


#include <execution>


namespace multio::action {

std::vector<std::unique_ptr<Operation>> make_operations(const std::vector<std::string>& opNames,
                                                        const message::Message& msg,
                                                        std::shared_ptr<StatisticsIO>& IOmanager,
                                                        const OperationWindow& win,
                                                        const StatisticsConfiguration& cfg) {

    return multio::util::dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        std::vector<std::unique_ptr<Operation>> stats;
        for (const auto& op : opNames) {
            switch (cfg.opPrecision()) {
                case (operationPrecision::ENFORCE_DOUBLE): {
                    switch ( cfg.executionPolicy()){
                        case (Policy::seq):{
                            stats.push_back(make_operation<double, Precision, std::execution::sequenced_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        case (Policy::unseq):{
                            stats.push_back(make_operation<double, Precision, std::execution::unsequenced_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        case (Policy::par_unseq):{
                            stats.push_back(make_operation<double, Precision, std::execution::parallel_unsequenced_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        case (Policy::par):{
                            stats.push_back(make_operation<double, Precision, std::execution::parallel_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        default:{
                            throw eckit::SeriousBug("ERROR: Wrong execution par", Here());            
                        }
                    }
                    break;
                }
                case (operationPrecision::ENFORCE_SINGLE): {
                    switch ( cfg.executionPolicy()){
                        case (Policy::seq):{
                            stats.push_back(make_operation<float, Precision,std::execution::sequenced_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        case (Policy::unseq):{
                            stats.push_back(make_operation<float, Precision,std::execution::unsequenced_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        case (Policy::par_unseq):{
                            stats.push_back(make_operation<float, Precision,std::execution::parallel_unsequenced_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        case (Policy::par):{
                            stats.push_back(make_operation<float, Precision, std::execution::parallel_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        default:{
                            throw eckit::SeriousBug("ERROR: Wrong execution par", Here());            
                        }
                    }
                    break;
                }
                case (operationPrecision::FROM_MESSAGE): {
                    switch ( cfg.executionPolicy()){
                        case (Policy::seq):{
                            stats.push_back(make_operation<Precision, Precision, std::execution::sequenced_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        case (Policy::unseq):{
                            stats.push_back(make_operation<Precision, Precision, std::execution::unsequenced_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        case (Policy::par_unseq):{
                            stats.push_back(make_operation<Precision, Precision, std::execution::parallel_unsequenced_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        case (Policy::par):{
                            stats.push_back(make_operation<Precision, Precision, std::execution::parallel_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        default:{
                            throw eckit::SeriousBug("ERROR: Wrong execution par", Here());            
                        }
                    }
                    break;
                }
            }
            if (!cfg.solver_send_initial_condition() && stats.back()->needStepZero()) {
                std::ostringstream os;
                os << " + Solver doesn't send initial condition and Operation needs it :: " << op << std::endl;
                throw eckit::SeriousBug(os.str(), Here());
            }
            if (cfg.solver_send_initial_condition()) {
                stats.back()->init(msg.payload());
            }
        }
        return stats;
    });
};


std::vector<std::unique_ptr<Operation>> make_operations(const std::string& op, size_t N, const message::Message& msg,
                                                        std::shared_ptr<StatisticsIO>& IOmanager,
                                                        const OperationWindow& win,
                                                        const StatisticsConfiguration& cfg) {

    return multio::util::dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        std::vector<std::unique_ptr<Operation>> stats;
        for (int i = 0; i < N; ++i) {
            switch (cfg.opPrecision()) {
                case (operationPrecision::ENFORCE_DOUBLE): {
                    switch ( cfg.executionPolicy()){
                        case (Policy::seq):{
                            stats.push_back(make_operation<double, Precision, std::execution::sequenced_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        case (Policy::unseq):{
                            stats.push_back(make_operation<double, Precision, std::execution::unsequenced_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        case (Policy::par_unseq):{
                            stats.push_back(make_operation<double, Precision, std::execution::parallel_unsequenced_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        case (Policy::par):{
                            stats.push_back(make_operation<double, Precision, std::execution::parallel_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        default:{
                            throw eckit::SeriousBug("ERROR: Wrong execution par", Here());            
                        }
                    }
                    break;
                }
                case (operationPrecision::ENFORCE_SINGLE): {
                    switch ( cfg.executionPolicy()){
                        case (Policy::seq):{
                            stats.push_back(make_operation<float, Precision, std::execution::sequenced_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        case (Policy::unseq):{
                            stats.push_back(make_operation<float, Precision, std::execution::unsequenced_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        case (Policy::par_unseq):{
                            stats.push_back(make_operation<float, Precision, std::execution::parallel_unsequenced_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        case (Policy::par):{
                            stats.push_back(make_operation<float, Precision, std::execution::parallel_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        default:{
                            throw eckit::SeriousBug("ERROR: Wrong execution par", Here());            
                        }
                    }
                    break;
                }
                case (operationPrecision::FROM_MESSAGE): {
                    switch ( cfg.executionPolicy()){
                        case (Policy::seq):{
                            stats.push_back(make_operation<Precision, Precision, std::execution::sequenced_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        case (Policy::unseq):{
                            stats.push_back(make_operation<Precision, Precision, std::execution::unsequenced_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        case (Policy::par_unseq):{
                            stats.push_back(make_operation<Precision, Precision, std::execution::parallel_unsequenced_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        case (Policy::par):{
                            stats.push_back(make_operation<Precision, Precision, std::execution::parallel_policy>(op, msg.size(), IOmanager, win, cfg));
                            break;
                        }
                        default:{
                            throw eckit::SeriousBug("ERROR: Wrong execution par", Here());            
                        }
                    }
                    break;
                }
            }
            if (!cfg.solver_send_initial_condition() && stats.back()->needStepZero()) {
                std::ostringstream os;
                os << " + Solver doesn't send initial condition and Operation needs it :: " << op << std::endl;
                throw eckit::SeriousBug(os.str(), Here());
            }
            if (cfg.solver_send_initial_condition()) {
                stats.back()->init(msg.payload());
            }
        }
        return stats;
    });
};


}  // namespace multio::action
