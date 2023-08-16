
#pragma once

#include <ostream>
#include <string>

#include "multio/action/statistics/OperationWindow.h"
#include "multio/action/statistics/StatisticsConfiguration.h"
#include "multio/action/statistics/StatisticsIO.h"
#include "multio/action/statistics/StatisticsProfiler.h"


#include "eckit/exception/Exceptions.h"

namespace multio::action {

class Operation {
public:
    Operation(const std::string& name, const std::string& operation, const OperationWindow& win,
              const StatisticsConfiguration& cfg) :
        name_{name}, operation_{operation}, logHeader_{"operation(" + name_ + ")"}, cfg_{cfg}, win_{win} {};

    virtual ~Operation() = default;

    const std::string& name() const { return name_; };
    const std::string& operation() const { return operation_; };

    virtual bool needStepZero() const = 0;
    virtual size_t memory_in_bytes() const = 0;
    virtual size_t byte_size() const = 0;
    virtual size_t numberOfStates() const = 0;


    virtual void init(const eckit::Buffer& data) = 0;
    virtual void updateData(const eckit::Buffer& data) = 0;
    virtual void updateWindow(const eckit::Buffer& data) = 0;
    virtual void compute(eckit::Buffer& data) const = 0;

    virtual void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) const = 0;
    virtual void load(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) = 0;

    int64_t getTotalTimeNsec(size_t idx ) const {
        if (idx > 5) {
            std::ostringstream os;
            os << "Index out of range" << std::endl;
            throw eckit::AssertionFailed(os.str());
        }
        return profiler_[idx].getTotalTimeNsec();
    };
    int64_t getNumberOfCalls(size_t idx ) const {
        if (idx > 5) {
            std::ostringstream os;
            os << "Index out of range" << std::endl;
            throw eckit::AssertionFailed(os.str());
        }
        return profiler_[idx].getNumberOfCalls();
    };

    void resetProfiler( ) const {
        for ( auto& p : profiler_){
            p.reset();
        }
        return;
    };

    virtual void init() = 0;

protected:
    virtual void print(std::ostream& os) const = 0;

    const std::string name_;
    const std::string operation_;
    const std::string logHeader_;
    const StatisticsConfiguration& cfg_;
    const OperationWindow& win_;
    mutable std::array<Profiler, 6> profiler_;

    friend std::ostream& operator<<(std::ostream& os, const Operation& a) {
        a.print(os);
        return os;
    }
};

}  // namespace multio::action
