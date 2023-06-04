#include "Operations.h"

namespace multio::action {

std::vector<std::unique_ptr<OperationBase>> make_operations(const std::vector<std::string>& opNames,
                                                            message::Message msg, StatisticsIO& IOmanager,
                                                            const StatisticsConfiguration& cfg, bool restart) {
    return multio::util::dispatchPrecisionTag(msg.precision(), [&](auto pt) {
        using Precision = typename decltype(pt)::type;
        ops_t stats;
        for (const auto& op : opNames) {
            stats.push_back(make_operation<Precision>(op, msg.size(), IOmanager, cfg, restart));
            if (cfg.solver_send_initial_condition()) {
                stats.back()->init(msg.payload().data(), msg.size());
            }
            else {
                stats.back()->init();
            }
        }
        return stats;
    });
};

}  // namespace multio::action