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

eckit::LocalConfiguration parseDisseminationFile(  const std::string& uri ) {
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

    return eckit::LocalConfiguration{};
}

eckit::LocalConfiguration
BuildDisseminationPlan(const eckit::LocalConfiguration& componentConfig, const MultioConfiguration& multioConf){

    // Get the name of the dissemination file
    std::string disseminationFile = multioConf.replaceCurly(componentConfig.getString("dissemination"));

    // generate requirements
    auto requirements = parseDisseminationFile( disseminationFile );

    // Generate requirements
    return eckit::LocalConfiguration{};    



};

}

eckit::LocalConfiguration
PlanBuilder(const eckit::LocalConfiguration& componentConfig, const MultioConfiguration& multioConf){

    if ( componentConfig.has("actions") ){
        return componentConfig;
    }
    else if ( componentConfig.has("dissemination") ){
        return BuildDisseminationPlan( componentConfig, multioConf );
    }
    else{
        std::ostringstream oss;
        oss << "Unable to build a plan with the provided configuration" << componentConfig;
        throw eckit::UserError(oss.str(), Here());        
    }

};

}