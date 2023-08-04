#pragma once

#include <cstdint>
#include <map>
#include <memory>
#include <string>
#include <vector>

#include "multio/action/ChainedAction.h"
#include "multio/message/Message.h"

#include "StatisticsConfiguration.h"

namespace multio::action {

// Match timestamp according to some specific rule that can be configured from
// input file
class SynopticMatcher {
public:
    SynopticMatcher(){};

    virtual ~SynopticMatcher(){};

    virtual size_t size() const = 0;

    virtual bool match(const message::Message& msg, const StatisticsConfiguration& cfg, size_t& key) const = 0;
};


class AllTimesMatcher final : public SynopticMatcher {
public:
    AllTimesMatcher(){};

    ~AllTimesMatcher(){};

    size_t size() const;

    bool match(const message::Message& msg, const StatisticsConfiguration& cfg, size_t& key) const;
};


class DailyHoursMatcher final : public SynopticMatcher {
public:
    DailyHoursMatcher(){};
    ~DailyHoursMatcher(){};

    size_t size() const;

    bool match(const message::Message& msg, const StatisticsConfiguration& cfg, size_t& key) const;
};


class DailyCustomMatcher final : public SynopticMatcher {
    const std::vector<long> reqHours_;

public:
    DailyCustomMatcher(const std::vector<long>& req) : reqHours_{req} {};
    ~DailyCustomMatcher(){};

    size_t size() const;

    bool match(const message::Message& msg, const StatisticsConfiguration& cfg, size_t& key) const;
};


// List of all available matchers
class SynopticMatchers {
private:
    std::map<std::string, std::shared_ptr<SynopticMatcher>> matchers_;

public:
    SynopticMatchers(const ComponentConfiguration& compConf);

    const std::shared_ptr<SynopticMatcher>& getMatcher(const std::string& key);
};

}  // namespace multio::action