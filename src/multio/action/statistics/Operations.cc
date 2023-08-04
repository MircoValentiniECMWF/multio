
#include "Operations.h"

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
                    stats.push_back(make_operation<double, Precision>(op, msg.size(), IOmanager, win, cfg));
                    break;
                }
                case (operationPrecision::ENFORCE_SINGLE): {
                    stats.push_back(make_operation<float, Precision>(op, msg.size(), IOmanager, win, cfg));
                    break;
                }
                case (operationPrecision::FROM_MESSAGE): {
                    stats.push_back(make_operation<Precision, Precision>(op, msg.size(), IOmanager, win, cfg));
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
                    stats.push_back(make_operation<double, Precision>(op, msg.size(), IOmanager, win, cfg));
                    break;
                }
                case (operationPrecision::ENFORCE_SINGLE): {
                    stats.push_back(make_operation<float, Precision>(op, msg.size(), IOmanager, win, cfg));
                    break;
                }
                case (operationPrecision::FROM_MESSAGE): {
                    stats.push_back(make_operation<Precision, Precision>(op, msg.size(), IOmanager, win, cfg));
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
