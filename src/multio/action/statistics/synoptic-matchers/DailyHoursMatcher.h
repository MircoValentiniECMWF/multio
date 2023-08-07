#pragma once

#include "multio/action/statistics/synoptic-matchers/Matcher.h"
#include "multio/action/statistics/StatisticsComponentsActivation.h"



#ifdef __ENABLE_DAILY_HOURS_MATCHER___

namespace multio::action {

class DailyHoursMatcher final : public SynopticMatcher {
public:
    DailyHoursMatcher(){};
    ~DailyHoursMatcher(){};

    size_t size() const {
        return static_cast<size_t>(24);
    };

    bool match(const message::Message& msg, const StatisticsConfiguration& cfg, size_t& key) const {
        eckit::DateTime now = currentDateTime(msg, cfg);
        key = (now.time().hhmmss() / 10000);
        return true;
    };
};

}

#endif