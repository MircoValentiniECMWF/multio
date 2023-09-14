#include "PlanBuilder.h"

namespace multio::config {

// eckit::LocalConfiguration
// PlanBuilder(const eckit::LocalConfiguration& componentConfig, const MultioConfiguration& multioConf){
// 
// };


eckit::LocalConfiguration
PlanBuilder(const eckit::LocalConfiguration& componentConfig, const MultioConfiguration& multioConf){

    if ( componentConfig.has("actions") ){
        return componentConfig;
    }
    // else if ( componentConfig.has("dissemination") ){
    //     return BuildDisseminationPlan( componentConfig, multioConf );
    // }
    else{
        std::ostringstream oss;
        oss << "Unable to build a plan with the provided configuration" << componentConfig;
        throw eckit::UserError(oss.str(), Here());        
    }

};

}