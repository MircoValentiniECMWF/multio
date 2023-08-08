#pragma once

#include <ostream>

#include "eckit/exception/Exceptions.h"
#include "multio/LibMultio.h"

#include "multio/action/statistics/synoptic-matchers/Matcher.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/action/statistics/StatisticsComponentsActivation.h"


#ifdef __ENABLE_DAILY_CUSTOM_MATCHER__

namespace multio::action {


namespace {


std::vector<long> initDailyCustomMatcherReadArray(const eckit::LocalConfiguration& compConf, const StatisticsConfiguration& cfg){
    std::vector<long> tmp;
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " + Initialize new DailyCustomMatcher with explicit configuration" << std::endl;
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " + Config    :: " <<  compConf << std::endl;
    tmp = compConf.getLongVector("hours-set" );
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
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " + Hours set :: " << tmp << std::endl;
    return tmp;
};


std::vector<long> initDailyCustomMatcherReadVars(const eckit::LocalConfiguration& compConf, const StatisticsConfiguration& cfg){
    std::vector<long> tmp;
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " + Initialize new DailyCustomMatcher with from/to/by configuration" << std::endl;
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " + Config    :: " <<  compConf << std::endl;
    long from;
    long to;
    long by;
    from = compConf.getLong("from");
    to   = compConf.getLong("to");
    by   = compConf.getLong("by",1);
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " + From    :: " <<  from << std::endl;
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " + To      :: " <<  to   << std::endl;
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " + By      :: " <<  by   << std::endl;
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
    LOG_DEBUG_LIB(LibMultio) << cfg.logPrefix() << " + Hours set :: " << tmp << std::endl;
    return tmp;
};


std::vector<long> initDailyCustomMatcher(const eckit::LocalConfiguration& compConf, const StatisticsConfiguration& cfg) {
    if (compConf.has("hours-set")) {
        return initDailyCustomMatcherReadArray( compConf, cfg );
    }

    if (compConf.has("from") && compConf.has("to") ) {
        return initDailyCustomMatcherReadVars( compConf, cfg );
    }

    std::ostringstream os;
    os << "Invalid configuration for the matcher :: DailyCustomMatcher" << std::endl;
    throw eckit::UserError(os.str(), Here());

};

} 


class DailyCustomMatcher final : public SynopticMatcher {

    const std::vector<long> reqHours_;

public:

    DailyCustomMatcher(const eckit::LocalConfiguration& compConf, const StatisticsConfiguration& cfg) : reqHours_{initDailyCustomMatcher(compConf,cfg)} {};
    ~DailyCustomMatcher(){};

    size_t size() const{
        return reqHours_.size();
    };


    bool match(const message::Message& msg, const StatisticsConfiguration& cfg, size_t& key) const {
        eckit::DateTime now = currentDateTime(msg, cfg);
        int tid = static_cast<int>(now.time().hhmmss() / 10000);
        bool found = false;
        key = 9999999;
        for ( int i=0; i<reqHours_.size(); ++i ){
            if ( tid == reqHours_[i] ){
                key = i;
                found = true;
            }
        };
        return found;
    };

};


}

#endif