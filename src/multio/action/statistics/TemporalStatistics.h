#pragma once

#include <string>

#include "eckit/types/DateTime.h"

#include "MovingWindow.h"
#include "Operations.h"
#include "StatisticsConfiguration.h"
#include "StatisticsIO.h"
#include "multio/message/Message.h"


namespace multio ::action {

class TemporalStatistics {
public:
    op::iterator begin() { return statistics_.begin(); };
    op::iterator end() { return statistics_.end(); };

    static std::unique_ptr<TemporalStatistics> build(const std::string& unit, long span,
                                                     const std::vector<std::string>& operations,
                                                     const message::Message& msg, const StatisticsConfiguration& cfg);

    TemporalStatistics(const std::vector<std::string>& operations, const DateTimePeriod& period,
                       const message::Message& msg, StatisticsIO& IOmanager, const StatisticsConfiguration& cfg,
                       long span);

    bool isEndOfWindow(message::Message& msg, const StatisticsConfiguration& cfg);
    bool isBeginOfWindow(message::Message& msg, const StatisticsConfiguration& cfg);

    void updateData(message::Message& msg, const StatisticsConfiguration& cfg);
    void updateWindow(const message::Message& msg, const StatisticsConfiguration& cfg);

    void dump(StatisticsIO& IOmanager, const StatisticsConfiguration& cfg) const;
    void load(StatisticsIO& IOmanager, const StatisticsConfiguration& cfg) const;

    const MovingWindow& win() const;

    void print(std::ostream& os) const;

private:
    const std::unique_ptr<PeriodUpdater> periodUpdater_;
    MovingWindow window_;
    std::vector<std::unique_ptr<OperationBase>> statistics_;

    friend std::ostream& operator<<(std::ostream& os, const TemporalStatistics& a) {
        a.print(os);
        return os;
    }
};

}  // namespace multio::action
