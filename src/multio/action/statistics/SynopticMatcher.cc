#include "SynopticMatcher.h"

#include <iostream>
#include <ostream>

#include "eckit/exception/Exceptions.h"
#include "eckit/types/DateTime.h"

#include "TimeUtils.h"
#include "multio/config/ComponentConfiguration.h"

namespace multio::action {
#if 0
namespace {

std::vector<long> initDailyCustomMatcher(const eckit::LocalConfiguration& cfg) {
    std::vector<long> tmp;
    if (cfg.has("hours-set")) {
        cfg.getlongVector("hours-set", tmp);
        std::sort(tmp.begin(), tmp.end());
        long = 25;
        for (auto& i : tmp) {
            if (i < 0 || i > 23) {
                throw eckit::SeriousBug{"Index out of range", Here()};
            }
            if ((old - i) == 0) {
                throw eckit::SeriousBug{"Duplicte are no allowed", Here()};
            }
        }
        return tmp
    }
    if (cfg.has("from") && cfg.has("to") && cfg.has("by")) {
        long from;
        long to;
        long by;
        cfg.getlong("from", from);
        cfg.getlong("to", to);
        cfg.getlong("by", by);
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
        return tmp
    }
}

}  // namespace
#endif
bool AllTimesMatcher::match(const message::Message& msg, const StatisticsConfiguration& cfg, size_t& key) const {
    key = 0;
    return true;
};


size_t AllTimesMatcher::size() const {
    return static_cast<size_t>(1);
};


bool DailyHoursMatcher::match(const message::Message& msg, const StatisticsConfiguration& cfg, size_t& key) const {
    eckit::DateTime now = currentDateTime(msg, cfg);
    key = (now.time().hhmmss() / 10000);
    return true;
};


size_t DailyHoursMatcher::size() const {
    return static_cast<size_t>(24);
};


bool DailyCustomMatcher::match(const message::Message& msg, const StatisticsConfiguration& cfg, size_t& key) const {
    eckit::DateTime now = currentDateTime(msg, cfg);
    int hh = (now.time().hhmmss() / 10000);
    for (int i = 0; i < reqHours_.size(); ++i) {
        if (reqHours_[i] == hh) {
            key = i;
            return true;
        }
    }
    return false;
};


size_t DailyCustomMatcher::size() const {
    return static_cast<size_t>(reqHours_.size());
};


SynopticMatchers::SynopticMatchers(const ComponentConfiguration& compConf) {
    // ush default matchers
    matchers_["NoFilter"] = std::make_shared<AllTimesMatcher>();
    matchers_["DailyHours"] = std::make_shared<DailyHoursMatcher>();
    //
#if 0    
    if (!compConf.parsedConfig().has("synoptic")) {
        return;
    }
    const auto& cfg = compConf.parsedConfig().getSubConfiguration("synoptic");
    bool loop = true;
    while (loop) {
        if (cfg.has("type")) {
            // Call the MAtchersFactory
        }
        else {
            throw eckit::SeriousBug{"Matcher configuration must have type", Here()};
        };
        if (cfg.has("next")) {
            cfg = cfg.subComponent("next");
        }
        else {
            loop = false;
        }
    }
#endif
    return;
};


const std::shared_ptr<SynopticMatcher>& SynopticMatchers::getMatcher(const std::string& key) {
    if (matchers_.find(key) != matchers_.end()) {
        return matchers_[key];
    }
    else {
        std::ostringstream os;
        os << "Wrong matcher: " << std::endl
           << "requested matcher is: \"" << key << "\"" << std::endl
           << "constructable matchers are: " << std::endl;
        for (auto it = matchers_.begin(); it != matchers_.end(); it++) {
            os << it->first << std::endl;
        }
        throw eckit::SeriousBug(os.str(), Here());
    }
};

}  // namespace multio::action