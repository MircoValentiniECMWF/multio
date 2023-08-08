
#include "multio/action/statistics/SynopticCollection.h"

#include <iostream>
#include <ostream>
#include <regex>


#include "multio/LibMultio.h"
#include "multio/action/statistics/OperationWindow.h"
#include "multio/action/statistics/Operations.h"
#include "multio/action/statistics/SynopticMatcher.h"


namespace multio::action {

namespace {


// Parse input for requested statistics
const std::array<std::string, 3> parseOperationName(
    const std::string& operation, const std::map<std::string, eckit::LocalConfiguration>& matcherConf) {
    static const std::regex op1_grammar("([a-zA-Z]+)::([a-zA-Z]+)");
    static const std::regex op2_grammar("([a-zA-Z]+)");
    std::smatch match1;
    std::smatch match2;
    std::array<std::string, 3> out;
    if (std::regex_match(operation, match1, op1_grammar)) {
        if (match1[1].str() == "NoFilter" || match1[1].str() == "DailyHours") {
            out[0] = match1[1].str();
            out[1] = match1[1].str();
            out[2] = match1[2].str();
            return out;
        }
        else if (matcherConf.find(match1[1].str()) != matcherConf.end()) {
            if (matcherConf.at(match1[1].str()).has("type")) {
                out[0] = match1[1].str();
                out[1] = matcherConf.at(match1[1].str()).getString("type");
                out[2] = match1[2].str();
                return out;
            }
            else {
                std::ostringstream os;
                os << "Wrong configuration for Operation :: " << std::endl
                   << "current configuration is: \"" << operation << "\""
                   << "valid configuration are : \"<synopticFilterName>::<OperationName>\" or "
                   << "\"<operationName>\"" << std::endl;
                throw eckit::SeriousBug(os.str(), Here());
            }
        }
        else {
            std::ostringstream os;
            os << "Wrong configuration for Operation :: " << std::endl
               << "current configuration is: \"" << operation << "\""
               << "valid configuration are : \"<synopticFilterName>::<OperationName>\" or "
               << "\"<operationName>\"" << std::endl;
            throw eckit::SeriousBug(os.str(), Here());
        }
    }
    else if (std::regex_match(operation, match2, op2_grammar)) {
        out[0] = "NoFilter";
        out[1] = "NoFilter";
        out[2] = match2[1].str();
        return out;
    }
    else {
        std::ostringstream os;
        os << "Wrong configuration for Operation :: " << std::endl
           << "current configuration is: \"" << operation << "\""
           << "valid configuration are : \"<synopticFilterName>::<OperationName>\" or "
           << "\"<operationName>\"" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
}
}  // namespace


// Construct a synoptic collection of statistics
SynopticCollection::SynopticCollection(const std::string& operation, const message::Message& msg,
                                       std::shared_ptr<StatisticsIO>& IOmanager, const OperationWindow& win,
                                       const std::map<std::string, eckit::LocalConfiguration>& matcherConf,
                                       const StatisticsConfiguration& cfg) :
    win_{win},
    op_{parseOperationName(operation, matcherConf)},
    matcher_{op_[0] == op_[1] ? make_matcher(op_[1], cfg) : make_matcher(op_[1], matcherConf.at(op_[0]), cfg)},
    statistics_{make_operations(op_[2], matcher_->size(), msg, IOmanager, win, cfg)} {};


// Number of different statistics that are contained in this collection
size_t SynopticCollection::size() const {
    LOG_DEBUG_LIB(::multio::LibMultio) << " *** SynopticCollection::size " << std::endl;
    return matcher_->size();
};


void SynopticCollection::resetWindow(const message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(::multio::LibMultio) << cfg.logPrefix() << " *** SynopticCollection::updateWindow " << std::endl;
    for (auto& stat : statistics_) {
        // TODO: Just set to zero all the data of the window
        // stat.resetWindow();
    }
    size_t key;
    if (matcher_->match(msg, cfg, key)) {
        // TODO: add profiling code
        // TODO: handling the time for a better metadata control
        // TODO: update data must have control over
        statistics_[key]->updateWindow(msg.payload());
    }
    return;
};


void SynopticCollection::updateData(const message::Message& msg, const StatisticsConfiguration& cfg) {
    LOG_DEBUG_LIB(::multio::LibMultio) << cfg.logPrefix() << " *** SynopticCollection::updateData " << std::endl;
    size_t key;
    if (matcher_->match(msg, cfg, key)) {
        // TODO: add profiling code
        // TODO: handling the time for a better metadata control
        statistics_[key]->updateData(msg.payload());
    }
    return;
};


void SynopticCollection::dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) const {
    LOG_DEBUG_LIB(::multio::LibMultio) << cfg.logPrefix() << " *** SynopticCollection::dump " << std::endl;
    for (auto& stat : statistics_) {
        stat->dump(IOmanager, cfg);
    }
    return;
};


const std::unique_ptr<Operation>& SynopticCollection::operator[](size_t idx) {
    if (idx >= statistics_.size()) {
        std::ostringstream os;
        os << "Index out of range :: " << std::endl
           << " - current value is :: " << idx << std::endl
           << " - valid range is   :: [" << 0 << "..." << size() - 1 << "]" << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    LOG_DEBUG_LIB(::multio::LibMultio) << " *** SynopticCollection::operator[" << idx << "]" << std::endl;
    return statistics_[idx];
}

#if 0
bool SynopticCollection::ready( size_t idx, const StatisticsConfiguration& cfg ){
    LOG_DEBUG_LIB(::multio::LibMultio) << cfg.logPrefix() << " *** SynopticCollection::updateData " << std::endl;
    size_t key;
    if ( matcher_->match( msg, cfg, key ) ){
        // TODO: add profiling code 
        // TODO: handling the time for a better metadata control
        statistics_[key]->updateData( msg.payload() );

    }
    return;
}


message::Message SynopticCollection::compute( size_t idx, const StatisticsConfiguration& cfg ){
    LOG_DEBUG_LIB(::multio::LibMultio) << cfg.logPrefix() << " *** SynopticCollection::compute " << std::endl;
    size_t key;
    if ( matcher_->match( msg, cfg, key ) ){
        // TODO: add profiling code 
        // TODO: handling the time for a better metadata control
        statistics_[key]->updateData( msg.payload() );
    }
    return;
}
#endif


std::vector<std::unique_ptr<SynopticCollection>> make_collections(
    const std::vector<std::string>& operations, const message::Message& msg, std::shared_ptr<StatisticsIO>& IOmanager,
    const OperationWindow& win, const std::map<std::string, eckit::LocalConfiguration>& matcherConf,
    const StatisticsConfiguration& cfg) {

    std::vector<std::unique_ptr<SynopticCollection>> collections;
    for (const auto& op : operations) {
        // std::string tmp = "DailyHours::" + op;
        // TODO: manage force double precision here
        collections.push_back(std::make_unique<SynopticCollection>(op, msg, IOmanager, win, matcherConf, cfg));
    }
    return collections;
}


}  // namespace multio::action