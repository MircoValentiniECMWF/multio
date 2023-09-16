#include "PlanBuilder.h"

#include <algorithm>
#include <vector>
#include <fstream>
#include <regex>

#include "eckit/exception/Exceptions.h"
#include "eckit/utils/StringTools.h"
#include "eckit/filesystem/URI.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/utils/Translator.h"

#include "metkit/mars/MarsRequest.h"
#include "metkit/mars/MarsParser.h"
#include "metkit/mars/MarsParsedRequest.h"

#include "multio/LibMultio.h"

using metkit::mars::MarsRequest;

namespace multio::config {

namespace{


// Not used
// static const std::set<std::string> retrieveKeys{
//     "class",
//     "stream",
//     "domain",
//     "expver",
//     "type",
//     "levtype",
//     "param",
//     "step",
//     "number",
//     "fcmonth",
//     "time",
//     "direction",
//     "frequency",
//     "levelist",
//     "method",
//     "quantile",
//     "system",
//     "use",
//     "origin"};
// 
// 
// static const std::set<std::string> postprocKeys{
//     "area",
//     "grid",
//     "resol",
//     "gaussian",
//     "frame",
//     "rotation",
//     "bitmap",
//     "accuracy",
//     "packing",
//     "padding",
//     "format",
//     "edition",
//     "interpolation"};
// 
// 
// static const std::set<std::string> sinkKeys{
//     "country",
//     "target",
//     "option"};


template< typename T >
T requestGet( const std::string& param, const MarsRequest& request ){
    T val;
    std::vector<std::string> values;
    if (request.getValues(param, values) == 1){
        val = eckit::Translator<std::string, T>{}(values[0]);
    }
    else {
        std::ostringstream oss;
        oss << "Scalar value expected";
        throw eckit::UserError(oss.str(), Here());   
    }
    return val;
}


std::vector<std::string> requestGetStringArray( const std::string& param, const MarsRequest& request ){
    std::vector<std::string> values;
    request.getValues(param, values);
    return values;
}


template< typename T >
std::vector<T> requestGetCastArray( const std::string& param, const MarsRequest& request ){
    std::vector<T> res;
    std::vector<std::string> values;
    request.getValues(param, values);
    for (  auto v : values ){
        res.push_back( eckit::Translator<std::string, T>{}(v) );
    }
    return res;
}


std::vector<long> requestParseLongArray( const std::string& param, const MarsRequest& request ){
    std::vector<long> val;
    std::vector<std::string> values;
    request.getValues(param, values);
    if ( values.size() > 2 && values[1] == "to" ){
        long from = eckit::Translator<std::string, long>{}( values[0] );
        long to   = eckit::Translator<std::string, long>{}( values[2] );
        long by   = 1;
        if ( values[3] == "by" ){
            by = eckit::Translator<std::string, long>{}( values[4] );
        }
        for ( long i=from; i<=to; i+=by){
            val.push_back( i );
        }
    }
    else{
        for ( auto v : values){
            val.push_back( eckit::Translator<std::string, long>{}(v) );
        }
    }
    return val;
}
 

eckit::LocalConfiguration parseDisseminationFile( const eckit::LocalConfiguration& componentConfig, const MultioConfiguration& multioConf, const metkit::mars::MarsParsedRequest& request, const std::string& planName ) {

    std::vector<std::string> params;
    request.getParams(params);

    // Get templates Path
    std::string templatesPath;
    if ( componentConfig.has("templates-path") ){
        templatesPath = multioConf.replaceCurly(componentConfig.getString("templates-path"));
        if ( !eckit::PathName{templatesPath}.exists() ){
            std::ostringstream oss;
            oss << "templates path not exists";
            throw eckit::UserError(oss.str(), Here());
        }
    }
    else {
        templatesPath = ".";
    }


    // Get templates Path
    std::string outputPath;
    if ( componentConfig.has("output-path") ){
        outputPath = multioConf.replaceCurly(componentConfig.getString("output-path"));
    }
    else {
        outputPath = ".";
    }
    eckit::PathName{outputPath}.mkdir();

    // The objective is to construct something like this starting from a metkit mars request
    //
    // In the select action more filter can (and should) be added
    //
    // LocalConfiguration[root={
    // name => reduced_gg_to_grid_0.50_0.50_area , 
    // actions => (
    //      {type => select , 
    //       match => (
    //          {param    => (133,134,135,136) , 
    //           levelist => (1,2,3,4,5,6,7) , 
    //           levtype  => ml
    //          }
    //        )
    //      },
    //
    //      {type => interpolate , 
    //       input => O1280 , 
    //       grid => (0.5,0.5) , 
    //       area => (80,0,-80,360) , 
    //       interpolation => linear , 
    //       options => 
    //         {caching => true}
    //      },
    //
    //      {type => encode , 
    //       format => grib , 
    //       template => /home/valentini/ecmwf/ifs-bundle/test-dissemination/MARS_reduced_gg_to_grid_0.50_0.50_area.grib
    //      },
    //
    //      {type => sink , 
    //       sinks => (
    //          {type => file , 
    //           append => false , 
    //           per-server => false , 
    //           path => MultIO_reduced_gg_to_grid_0.50_0.50_area.grib
    //      }
    //   )
    // }
     

    auto generate_select_action = [&](const eckit::LocalConfiguration& componentConfig) -> eckit::LocalConfiguration {
        eckit::LocalConfiguration select_action;
        eckit::LocalConfiguration matcher;
        std::vector<eckit::LocalConfiguration> matchers;
        if ( request.has("param") ){
            matcher.set("param",requestParseLongArray("param",request));
        }
        if ( request.has("levtype") ){
            matcher.set("levtype",requestGetStringArray("levtype",request));
        }
        if ( request.has("levelist") ){
            matcher.set("levelist",requestParseLongArray("levelist",request));
        }
        matchers.push_back(matcher);
        select_action.set("type","select");
        select_action.set("match",matchers);
        return select_action;
    };

    auto generate_interpolate_action = [&](const eckit::LocalConfiguration& componentConfig) -> eckit::LocalConfiguration {
        eckit::LocalConfiguration interpolate_action;
        eckit::LocalConfiguration interpolate_options;
        interpolate_options.set("caching", true );
        interpolate_action.set("type","interpolate");
        if ( componentConfig.has("input") ){
            interpolate_action.set("input",componentConfig.getString("input"));
        }
        else {
            std::ostringstream oss;
            oss << "Input keyword not present in the dissemination plan : " << componentConfig;
            throw eckit::UserError(oss.str(), Here());
        }

        if ( request.has("grid") ){
            std::vector<double> grid;
            interpolate_action.set("grid",requestGetCastArray<double>("grid",request));
        }
        else {
            std::ostringstream oss;
            oss << "unable to  find the \"grid\" parameter : " << componentConfig;
            throw eckit::UserError(oss.str(), Here());
        }
        if ( request.has("area") ){
            std::vector<long> area;
            interpolate_action.set("area",requestGetCastArray<double>("area",request));
        }
        if ( request.has("interpolation") ){

            interpolate_action.set("interpolation",requestGet<std::string>( "interpolation", request ));
        }
        interpolate_action.set("options",interpolate_options);
        return interpolate_action;
    };

    auto generate_encode_action = [&](const eckit::LocalConfiguration& componentConfig) -> eckit::LocalConfiguration {
        eckit::LocalConfiguration encode_action;
        encode_action.set("type","encode");
        encode_action.set("format","grib");
        if ( ! componentConfig.has("templates")){
            std::ostringstream oss;
            oss << "keyword \"templates\" not present";
            throw eckit::UserError(oss.str(), Here());
        }
        if ( ! request.has("levtype")){
            std::ostringstream oss;
            oss << "keyword \"levtype\" not present";
            throw eckit::UserError(oss.str(), Here());
        }
        std::vector<eckit::LocalConfiguration> templates = componentConfig.getSubConfigurations("templates");
        std::string levtype = requestGet<std::string>( "levtype", request );
        for ( auto& t : templates ){
            if ( t.has(levtype) ) {
                std::string templatName = outputPath + "/" + t.getString(levtype);
                encode_action.set("template", templatName );
            }
        }
        return encode_action;
    };

    auto generate_sink_action = [&](const eckit::LocalConfiguration& componentConfig) -> eckit::LocalConfiguration {
        eckit::LocalConfiguration sink_action;
        sink_action.set("type","sink");
        std::vector<eckit::LocalConfiguration> sinks;
        eckit::LocalConfiguration file_sink;
        file_sink.set("type","file");
        file_sink.set("append",false);
        file_sink.set("per-server",false);
        // Default name
        std::string fname = outputPath + "/" + planName + ".grib";
        file_sink.set("path",fname);
        // Override1 Name
        if ( componentConfig.has("output-name") ){
            std::string fname = outputPath + "/" + componentConfig.getString("output-name") + ".grib";
            file_sink.set("path",fname);
        }
        // Override2 Name
        if ( request.has("target") ){
            std::string fname = outputPath + "/" + planName + "-" + requestGet<std::string>( "target", request ) + ".grib";
            file_sink.set("path",fname);
        }
        sinks.push_back( file_sink );
        sink_action.set("type","sink");
        sink_action.set("sinks",sinks);
        return sink_action;
    };

    std::vector<eckit::LocalConfiguration> actions;

    actions.push_back( generate_select_action     ( componentConfig ) );
    actions.push_back( generate_interpolate_action( componentConfig ) );
    actions.push_back( generate_encode_action     ( componentConfig ) );
    actions.push_back( generate_sink_action       ( componentConfig ) );

    eckit::LocalConfiguration plan;

    plan.set( "name", planName );
    plan.set( "actions", actions );


    LOG_DEBUG_LIB(multio::LibMultio) << plan << std::endl;

    return plan;
}



std::vector<eckit::LocalConfiguration> parseDisseminationFiles( const eckit::LocalConfiguration& componentConfig, const MultioConfiguration& multioConf, const std::string& uri ) {
    // std::vector<Requirement> requirements;
    std::ifstream file(uri);
    if (!file) {
        throw eckit::CantOpenFile(uri, Here());
    }
    metkit::mars::MarsParser parser(file);
    std::vector<metkit::mars::MarsParsedRequest> requests = parser.parse();

    if ( !componentConfig.has("name") ){
        std::ostringstream oss;
        oss << "keyword \"name\" not present in the dissemination plan";
        throw eckit::UserError(oss.str(), Here());
    }

    std::vector<eckit::LocalConfiguration> plans;
    long cnt = 0;
    for ( auto& request : requests ){
        std::string planName =  componentConfig.getString("name") + "_" + std::to_string( cnt++ );
        plans.push_back( parseDisseminationFile( componentConfig, multioConf, request, planName ) );
    }

    return plans;

}


std::vector<eckit::LocalConfiguration> optimizeDisseminationPlans( const std::vector<eckit::LocalConfiguration>& rawPlans ){
    // Maybe, Oneday, Someone, ...
    return rawPlans;
}


std::vector<eckit::LocalConfiguration>
BuildDisseminationPlan(const eckit::LocalConfiguration& componentConfig, const MultioConfiguration& multioConf){

    // Get the name of the dissemination file
    std::string disseminationFile = multioConf.replaceCurly(componentConfig.getString("dissemination"));
    
    // Create plans
    std::vector<eckit::LocalConfiguration> rawPlans = parseDisseminationFiles( componentConfig, multioConf, disseminationFile );

    // Optimize Dissemination graphs
    std::vector<eckit::LocalConfiguration> optimizedPlans = optimizeDisseminationPlans(rawPlans);

    // Generate requirements
    return optimizedPlans;

};

}  // namespace


std::vector<eckit::LocalConfiguration>
PlanBuilder(const eckit::LocalConfiguration& componentConfig, const MultioConfiguration& multioConf){

    // Read nested files
    eckit::LocalConfiguration cfg;
    if ( componentConfig.has("file") ){
        const auto& file = multioConf.getConfigFile(
            multioConf.replaceCurly(componentConfig.getString("file")));
        cfg = file.content;
    }
    else {
        cfg = componentConfig;
    }

    // Create the output plans
    if ( cfg.has("actions") ){
        LOG_DEBUG_LIB(multio::LibMultio) << cfg << std::endl;
        return std::vector<eckit::LocalConfiguration>{cfg};
    }
    
    if ( cfg.has("dissemination") ){
        LOG_DEBUG_LIB(multio::LibMultio) << cfg << std::endl;
        return BuildDisseminationPlan( cfg, multioConf );
    }
    
    // Error
    std::ostringstream oss;
    oss << "Unable to build a plan with the provided configuration : " << cfg;
    throw eckit::UserError(oss.str(), Here());        
    

};

}