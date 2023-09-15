#include "PlanBuilder.h"

#include <algorithm>
#include <vector>
#include <fstream>

#include "eckit/exception/Exceptions.h"
#include "eckit/utils/StringTools.h"
#include "eckit/filesystem/URI.h"

#include "metkit/mars/MarsRequest.h"
#include "metkit/mars/MarsParser.h"
#include "metkit/mars/MarsParsedRequest.h"

using metkit::mars::MarsRequest;

namespace multio::config {

namespace{

typedef std::vector<std::pair<std::string, std::vector<std::string> > > Expand;


static const std::set<std::string> retrieveKeys{"class",
    "stream",
    "domain",
    "expver",
    "type",
    "levtype",
    "param",
    "step",
    "number",
    "fcmonth",
    "time",
    "direction",
    "frequency",
    "levelist",
    "method",
    "quantile",
    "system",
    "use",
    "origin"};

static const std::set<std::string> postprocKeys{"area",
    "grid",
    "resol",
    "gaussian",
    "frame",
    "rotation",
    "bitmap",
    "accuracy",
    "packing",
    "padding",
    "format",
    "edition",
    "interpolation"};

static const std::set<std::string> sinkKeys{"country",
    "target",
    "option"};

static void fillMap(std::map<std::string, std::string>& map,
                    const std::set<std::string>& keys,
                    const std::vector<std::string>& params,
                    const MarsRequest& request) {
    std::vector<std::string> values;
    for (auto& param : params) {
        if (keys.find(param) != keys.end()) {
            if (request.getValues(param, values) == 1) {
                map[param] = values[0];
            } else {
                // Not expanding values here, so need to rejoin values split when parsing
                map[param] = eckit::StringTools::join("/", values);
            }
        }
    }
}

static void fillMap(std::map<std::string, std::string>& map,
                    Expand& expand,
                    const std::set<std::string>& keys,
                    const std::vector<std::string>& params,
                    const MarsRequest& request) {
    std::vector<std::string> values;
    for (auto& param : params) {
        if (keys.find(param) != keys.end()) {
            if (request.getValues(param, values) == 1) {
                map[param] = values[0];
            } else {
                expand.push_back(std::make_pair(param, values));
            }
        }
    }
}

// static void generateRequirements(std::vector<Requirement>& requirements,
//                                  std::map<std::string, std::string>& retrieve,
//                                  std::vector<eckit::URI>& uris,
//                                  Expand& expand,
//                                  const std::map<std::string, std::string>& postproc,
//                                  const std::map<std::string, std::string>& sink,
//                                  size_t priority,
//                                  size_t level = 0) {
//     if (level >= expand.size()) {
//         std::vector<std::map<std::string, std::string> > handlers;
//         std::map<std::string, std::string> metadata;
//         requirements.push_back(Requirement(retrieve, uris, postproc, sink, handlers, metadata, priority));
//         return;
//     }
//     const auto& values = expand[level];
//     for (const auto& value : values.second) {
//         retrieve[values.first] = value;
//         generateRequirements(requirements, retrieve, uris, expand, postproc, sink, priority, level + 1);
//     }
// }

// class Requirement;

eckit::LocalConfiguration parseDisseminationFile( const eckit::LocalConfiguration& componentConfig, const std::string& uri ) {
    // std::vector<Requirement> requirements;
    std::ifstream file(uri);
    if (!file) {
        throw eckit::CantOpenFile(uri, Here());
    }
    metkit::mars::MarsParser parser(file);
    std::vector<metkit::mars::MarsParsedRequest> requests = parser.parse();

    ASSERT(requests.size() == 1);

    MarsRequest& request = requests[0];

    std::map<std::string, std::string> retrieve;
    std::vector<eckit::URI> uris;
    Expand expand;
    std::map<std::string, std::string> postproc;
    std::map<std::string, std::string> sink;

    std::vector<std::string> params;
    request.getParams(params);

    fillMap(retrieve, expand, retrieveKeys, params, request);
    fillMap(postproc, postprocKeys, params, request);
    fillMap(sink, sinkKeys, params, request);

    // Set defaults for retrieve
    // TODO: these are overrides rather than defaults, is that intended?
    // for (const auto& it : forceRetrieve_)
    //     retrieve[it.first] = it.second;

    // TODO: handle includeLine, bitmaps, wparams

    // generateRequirements(requirements, retrieve, uris, expand, postproc, sink, 0);

    auto generate_select_action = [&](const eckit::LocalConfiguration& componentConfig) -> eckit::LocalConfiguration {
        eckit::LocalConfiguration select_action;
        select_action.set("type","select");
        select_action.set("param",133);
        return select_action;
    };

    auto generate_interpolate_action = [&](const eckit::LocalConfiguration& componentConfig) -> eckit::LocalConfiguration {
        eckit::LocalConfiguration interpolate_action;
        eckit::LocalConfiguration interpolate_options;
        interpolate_options.set("caching", true );
        interpolate_action.set("type","interpolate");
        interpolate_action.set("input","O1280");
        std::vector<double> grid={0.5,0.5};
        interpolate_action.set("grid",grid);
        std::vector<long> area={80,0,-80,360};
        interpolate_action.set("area",area);
        interpolate_action.set("interpolation","linear");
        interpolate_action.set("options",interpolate_options);
        return interpolate_action;
    };

    auto generate_encode_action = [&](const eckit::LocalConfiguration& componentConfig) -> eckit::LocalConfiguration {
        eckit::LocalConfiguration encode_action;
        encode_action.set("type","encode");
        encode_action.set("format","grib");
        encode_action.set("template","MARS_reduced_gg_to_grid_0.50_0.50_area.grib");
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
        file_sink.set("path","MultIO_reduced_gg_to_grid_0.50_0.50_area.grib");
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

    plan.set( "name", "test" );
    plan.set( "actions", actions );


    std::cout << plan << std::endl;

    return plan;
}

eckit::LocalConfiguration
BuildDisseminationPlan(const eckit::LocalConfiguration& componentConfig, const MultioConfiguration& multioConf){

    // Get the name of the dissemination file
    std::string disseminationFile = multioConf.replaceCurly(componentConfig.getString("dissemination"));
    
    // Generate requirements
    return parseDisseminationFile( componentConfig, disseminationFile );



};

}

eckit::LocalConfiguration
PlanBuilder(const eckit::LocalConfiguration& componentConfig, const MultioConfiguration& multioConf){

    if ( componentConfig.has("actions") ){
        std::cout << componentConfig << std::endl;
        return componentConfig;
    }
    else if ( componentConfig.has("dissemination") ){
        return BuildDisseminationPlan( componentConfig, multioConf );
    }
    else{
        std::ostringstream oss;
        oss << "Unable to build a plan with the provided configuration : " << componentConfig;
        throw eckit::UserError(oss.str(), Here());        
    }

};

}