#pragma once

#include <string>

#include "multio/message/Message.h"

#include "OperationWindow.h"
#include "Operations.h"
#include "PeriodUpdater.h"
#include "StatisticsConfiguration.h"
#include "StatisticsIO.h"
#include "SynopticCollection.h"
#include "SynopticMatcher.h"


namespace multio::action {

class TemporalStatistics {
public:
    TemporalStatistics(const std::shared_ptr<PeriodUpdater>& periodUpdater,
                       const std::vector<std::string>& operations,
                       const message::Message& msg,
                       std::shared_ptr<StatisticsIO>& IOmanager,
                       const std::map<std::string,eckit::LocalConfiguration>& matcherConf,
                       const StatisticsConfiguration& cfg);

    bool isEndOfWindow(message::Message& msg, const StatisticsConfiguration& cfg);

    void updateData(message::Message& msg, const StatisticsConfiguration& cfg);
    void updateWindow(const message::Message& msg, const StatisticsConfiguration& cfg);

    void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) const;

    const OperationWindow& cwin() const;
    OperationWindow& win();

    void print(std::ostream& os) const;


    struct Iterator {
        using iterator_category = std::forward_iterator_tag;
        using difference_type = std::ptrdiff_t;
        using value_type = int;
        using pointer = const std::unique_ptr<Operation>&;
        using reference = const std::vector<std::unique_ptr<SynopticCollection>>&;

        Iterator(reference coll, size_t topIdx, size_t subIdx) : coll_(coll), topIdx_(topIdx), subIdx_(subIdx) {}

        // reference operator*() const { return *coll_[topIdx][subIdx]; }
        pointer operator*() {
            // std::cout << topIdx_ << ", " << subIdx_ << std::endl;
            return (*coll_[topIdx_])[subIdx_];
        }
        Iterator& operator++() {
            subIdx_++;
            if (subIdx_ >= coll_[topIdx_]->size()) {
                subIdx_ = 0;
                topIdx_++;
            }
            return *this;
        }

        friend bool operator==(const Iterator& a, const Iterator& b) {
            return (a.topIdx_ == b.topIdx_) && (a.subIdx_ == b.subIdx_);
        };

        friend bool operator!=(const Iterator& a, const Iterator& b) {
            bool ret = (a.topIdx_ != b.topIdx_) || (a.subIdx_ != b.subIdx_);
            return ret;
        };

    private:
        reference coll_;
        size_t topIdx_;
        size_t subIdx_;
    };


    Iterator collection_begin() { 
        return Iterator(collections_, 0, 0);
    };

    
    Iterator collection_end() {
        size_t topIdx = collections_.size();
        size_t subIdx = 0;
        return Iterator(collections_, topIdx, subIdx);
    };

private:
    const std::shared_ptr<PeriodUpdater>& periodUpdater_;
    OperationWindow window_;
    std::vector<std::unique_ptr<SynopticCollection>> collections_;

    friend std::ostream& operator<<(std::ostream& os, const TemporalStatistics& a) {
        a.print(os);
        return os;
    }
};

}  // namespace multio::action
