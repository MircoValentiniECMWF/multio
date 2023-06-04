#pragma once

#include "StatisticsConfiguration.h"
#include "TemporalStatistics.h"

namespace multio::action {

class DailyStatistics : public TemporalStatistics {
public:
    DailyStatistics(const std::vector<std::string> operations, long span, message::Message msg,
                    const std::string& partialPath, const StatisticsConfiguration& cfg);
    void resetPeriod(const message::Message& msg, const StatisticsConfiguration& cfg) override;
    void print(std::ostream& os) const override;
};
}  // namespace multio::action