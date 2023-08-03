#pragma once


#include <string>
#include <memory>
#include <vector>
#include <cstdint>

#include "multio/message/Message.h"
#include "multio/action/ChainedAction.h"


#include "StatisticsIO.h"
#include "OperationWindow.h"
#include "SynopticMatcher.h"
#include "StatisticsConfiguration.h"
#include "Operations.h"


namespace multio::action {
// Single operation (i.e. average, stddev) but all the possible "flavours"
// A "flavour" is a subset of the available samples
// 
// i.e. [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
// 
class SynopticCollection {
    private:

        // Reference to the parent object
        const OperationWindow& win_;

        // Kind of operation
        const std::array<std::string,2> op_;

        // Pointer to the matcher used to construct the operation-flavours
        const std::shared_ptr<SynopticMatcher> matcher_;


        // memory space used to save the data of partial operations
        std::vector<std::unique_ptr<Operation>> statistics_;

    public:


        SynopticCollection(  const std::string&               operation,
                                   SynopticMatchers&          matchers, 
                             const message::Message&          msg,
                             std::shared_ptr<StatisticsIO>&   IOmanager,
                             const OperationWindow&           win, 
                             const StatisticsConfiguration&   cfg );


        std::unique_ptr<Operation>& operator[]( size_t idx );

        size_t size() const;

        void resetWindow ( const message::Message& msg, const StatisticsConfiguration& cfg );
        void updateData  ( const message::Message& msg, const StatisticsConfiguration& cfg );

};

}