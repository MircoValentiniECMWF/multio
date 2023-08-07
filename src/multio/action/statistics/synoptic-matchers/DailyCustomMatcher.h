#pragma once

#include <ostream>

#include "eckit/exception/Exceptions.h"

#include "multio/action/statistics/synoptic-matchers/Matcher.h"


namespace multio::action {


namespace {

std::vector<long> initDailyCustomMatcher(const eckit::LocalConfiguration& cfg) {
    std::vector<long> tmp;
    if (cfg.has("hours-set")) {
        cfg.getLongVector("hours-set", tmp);
        std::sort(tmp.begin(), tmp.end());
        long old = -1;
        for (auto& i : tmp) {
            if (i < 0 || i > 23) {
                throw eckit::SeriousBug{"Index out of range", Here()};
            }
            if ((old - i) == 0) {
                throw eckit::SeriousBug{"Duplicte are no allowed", Here()};
            }
            old = i;
        }
        return tmp;
    }
    if (cfg.has("from") && cfg.has("to") && cfg.has("by")) {
        long from;
        long to;
        long by;
        cfg.getLong("from", from);
        cfg.getLong("to", to);
        cfg.getLong("by", by);
        if (by < 1 || by > 23) {
            throw eckit::SeriousBug{"\"by\" out of range", Here()};
        }
        if (from < 0 || from > 23) {
            throw eckit::SeriousBug{"\"to\" out of range", Here()};
        }
        if (to < 0 || to > 23) {
            throw eckit::SeriousBug{"\"from\" out of range", Here()};
        }
        for (long i = from; i <= to; i = i + by) {
            tmp.push_back(i);
        }
        return tmp;
    }

    std::ostringstream os;
    os << "Invalid configuration for DailyCustomMatcher" << std::endl;
    throw eckit::UserError(os.str(), Here());

};

} 


class DailyCustomMatcher final : public SynopticMatcher {
    const std::vector<long> reqHours_;

public:
    DailyCustomMatcher(const eckit::LocalConfiguration& cfg) : reqHours_{initDailyCustomMatcher(cfg)} {};
    ~DailyCustomMatcher(){};

    size_t size() const;

    bool match(const message::Message& msg, const StatisticsConfiguration& cfg, size_t& key) const;
};


}