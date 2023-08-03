#include "SynopticMatcher.h"

#include <iostream>
#include <ostream>

#include "eckit/exception/Exceptions.h"
#include "eckit/types/DateTime.h"

#include "TimeUtils.h"

namespace multio::action {

bool AllTimesMatcher::match(const message::Message& msg, const StatisticsConfiguration& cfg, size_t& key ) const {
    key = 0;
    return true;
};


size_t AllTimesMatcher::size() const
{
    return static_cast<size_t>(1);
};


bool DailyHoursMatcher::match( const message::Message& msg, const StatisticsConfiguration& cfg, size_t& key ) const {
    eckit::DateTime now = currentDateTime(msg, cfg);
    key = (now.time().hhmmss()/10000);
    return true;
};


size_t DailyHoursMatcher::size() const
{
    return static_cast<size_t>(24);
};


SynopticMatchers::SynopticMatchers( const ComponentConfiguration& compConf ){
    // ush default matchers
    matchers_["NoFilter"]   = std::make_shared<AllTimesMatcher>( );
    matchers_["DailyHours"] = std::make_shared<DailyHoursMatcher>(  );
    // TODO: Add new matchers from configuration to the list of used matchers
};


const std::shared_ptr<SynopticMatcher>& SynopticMatchers::getMatcher( const std::string& key ) {
    if ( matchers_.find(key) != matchers_.end() ){
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

}