#pragma once

#include "multio/action/statistics/synoptic-matchers/Matcher.h"
#include "multio/action/statistics/StatisticsComponentsActivation.h"

#ifdef ENABLE_NO_FILTER_MATCHER

namespace multio::action {

class AllTimesMatcher final : public SynopticMatcher {
public:
    AllTimesMatcher(){};

    ~AllTimesMatcher(){};

    size_t size() const {
        return static_cast<size_t>(1);
    };


    bool match(const message::Message& msg, const StatisticsConfiguration& cfg, size_t& key) const{
        key = 0;
        return true;
    };
};

}

#endif