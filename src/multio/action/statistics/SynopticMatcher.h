#pragma once

#include <cstdint>
#include <map>
#include <memory>
#include <string>
#include <vector>


#include "multio/action/ChainedAction.h"
#include "multio/message/Message.h"
#include "StatisticsConfiguration.h"
#include "multio/action/statistics/synoptic-matchers/Matcher.h"
#include "multio/config/ComponentConfiguration.h"



namespace multio::action {

std::unique_ptr<SynopticMatcher> make_matcher( const std::string& matcherKind, const StatisticsConfiguration& cfg  );
std::unique_ptr<SynopticMatcher> make_matcher( const std::string& matcherKind,  const eckit::LocalConfiguration& compConf, const StatisticsConfiguration& cfg  );


}  // namespace multio::action