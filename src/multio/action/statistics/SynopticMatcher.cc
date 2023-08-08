#include "SynopticMatcher.h"

#include <iostream>
#include <ostream>

#include "eckit/exception/Exceptions.h"
#include "eckit/types/DateTime.h"

#include "multio/action/statistics/TimeUtils.h"
#include "multio/action/statistics/StatisticsComponentsActivation.h"
#include "multio/config/ComponentConfiguration.h"




#include "multio/action/statistics/synoptic-matchers/AllTimesMatcher.h"
#include "multio/action/statistics/synoptic-matchers/DailyHoursMatcher.h"
#include "multio/action/statistics/synoptic-matchers/DailyCustomMatcher.h"

namespace multio::action {

std::unique_ptr<SynopticMatcher> make_matcher( const std::string& matcherKind, const StatisticsConfiguration& cfg  ){

#ifdef __ENABLE_NO_FILTER_MATCHER__
    if ( matcherKind == "NoFilter" ){
        return std::make_unique<AllTimesMatcher>(  );
    }
#endif

#ifdef __ENABLE_DAILY_HOURS_MATCHER__
    if ( matcherKind == "DailyHours" ){
        return std::make_unique<DailyHoursMatcher>( );        
    }
#endif

    std::ostringstream os;
    os << "Invalid matcher name in statistics :: " << matcherKind << std::endl;
    throw eckit::UserError(os.str(), Here());

};


std::unique_ptr<SynopticMatcher> make_matcher( const std::string& matcherKind, const eckit::LocalConfiguration& compConf, const StatisticsConfiguration& cfg  ){

#ifdef __ENABLE_DAILY_CUSTOM_MATCHER__
    if ( matcherKind == "DailyCustom" ){
        return std::make_unique<DailyCustomMatcher>( compConf, cfg );               
    }
#endif

    std::ostringstream os;
    os << "Invalid matcher name in statistics :: " << matcherKind << std::endl;
    throw eckit::UserError(os.str(), Here());

};

}  // namespace multio::action