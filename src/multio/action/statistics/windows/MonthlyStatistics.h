#pragma once

#include "StatisticsConfiguration.h"
#include "TemporalStatistics.h"

namespace multio::action {

class MonthlyStatistics : public TemporalStatistics {
public:
    MonthlyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                      const std::string& partialPath, const StatisticsConfiguration& options);
    void resetPeriod(const message::Message& msg, const StatisticsConfiguration& options) override;
    void print(std::ostream& os) const override;
};

}  // namespace multio::action