#pragma once

#include <cstdint>


#include "multio/message/Message.h"
#include "eckit/config/LocalConfiguration.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/action/statistics/StatisticsConfiguration.h"


namespace multio::action {

class SynopticMatcher {
public:
    SynopticMatcher(){};

    virtual ~SynopticMatcher(){};

    virtual size_t size() const = 0;

    virtual bool match(const message::Message& msg, const StatisticsConfiguration& cfg, size_t& key) const = 0;
};

}
