#include "TemporalStatistics.h"

namespace multio {
namespace action {


class HourlyStatistics : public TemporalStatistics {
public:
    HourlyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                     const std::string& partialPath, const StatisticsOptions& options);
    void resetPeriod(const message::Message& msg) override;
    void print(std::ostream& os) const override;
};

}  // namespace action
}  // namespace multio