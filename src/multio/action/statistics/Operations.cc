
#include "Operations.h"

#include "multio/action/statistics/StatisticsConfiguration.h"

namespace multio::action {

std::vector<std::unique_ptr<Operation>> make_operations(const std::string& op, size_t N, const message::Message& msg,
                                                        std::shared_ptr<StatisticsIO>& IOmanager,
                                                        const OperationWindow& win,
                                                        const StatisticsConfiguration& cfg) {


    auto evalPrecision = [&msg](operationPrecision opPrec) {
        switch (opPrec) {
            case operationPrecision::ENFORCE_DOUBLE:
                return util::PrecisionTag::Double;
            case operationPrecision::ENFORCE_SINGLE:
                return util::PrecisionTag::Float;
            default:
                return msg.precision();
        }
    };

    std::vector<std::unique_ptr<Operation>> stats;
    util::dispatchPrecisionTag(msg.precision(), [&](auto ptMsg) {
        util::dispatchPrecisionTag(evalPrecision(cfg.opPrecision()), [&](auto ptNum) {
            using PrecisionMsg = typename decltype(ptMsg)::type;
            using PrecisionNumerics = typename decltype(ptNum)::type;

            for (int i = 0; i < N; ++i) {
                stats.push_back(make_operation<PrecisionNumerics, PrecisionMsg>(op, msg.size(), IOmanager, win, cfg));
                if (!cfg.solver_send_initial_condition() && stats.back()->needStepZero()) {
                    std::ostringstream os;
                    os << " + Solver doesn't send initial condition and Operation needs it :: " << op << std::endl;
                    throw eckit::SeriousBug(os.str(), Here());
                }
                if (cfg.solver_send_initial_condition()) {
                    stats.back()->init(msg.payload());
                }
            }
        });
    });
    return stats;
}


}  // namespace multio::action
