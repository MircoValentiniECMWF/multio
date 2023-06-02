#include "multio/action/statistics/TemporalStatistics.h"

namespace multio::action {

class HourlyStatistics : public TemporalStatistics {
public:
    HourlyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                     const std::string& partialPath, StatisticsOptions& options);
    void resetPeriod(const message::Message& msg, StatisticsOptions& options) override;
    void print(std::ostream& os) const override;
};

}  // namespace action