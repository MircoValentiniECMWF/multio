#pragma once


#include "eckit/config/LocalConfiguration.h"

#include "multio/config/MultioConfiguration.h"


namespace multio::config {

eckit::LocalConfiguration PlanBuilder(const eckit::LocalConfiguration& componentConfig, const MultioConfiguration& multioConf);

}
