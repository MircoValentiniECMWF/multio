#pragma once

#include <cmath>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/io/Buffer.h"
#include "eckit/log/Log.h"
#include "eckit/types/DateTime.h"


#include "multio/LibMultio.h"
#include "multio/action/statistics/StatisticsOptions.h"
#include "multio/action/statistics/StatisticsIO.h"
#include "multio/util/VariantHelpers.h"

namespace multio::action {

//==== Base class =================================
class Operation {
public:
    Operation(const std::string& name, const std::string& operation, StatisticsOptions& options) :
        name_{name}, operation_{operation}, logHeader_{"operation(" + name_ + ")"}, options_{options} {}
    const std::string& name() const { return name_; }
    const std::string& operation() const { return operation_; }

    virtual void update(const void* val, long sz, eckit::DateTime dt) = 0;

    virtual ~Operation() = default;

    virtual void load(const std::string& partialPath ) = 0;
    virtual void dump(const std::string& partialPath, const long step) const = 0;

    virtual size_t byte_size() const = 0;
    virtual void compute( eckit::Buffer& buf ) = 0;
    virtual void reset( const void* data, long sz, eckit::DateTime dt ) = 0;
    virtual void init( const void* data, long sz, eckit::DateTime dt ) = 0;
    virtual void init( eckit::DateTime dt ) = 0;

protected:
    virtual void print(std::ostream& os) const = 0;

    const std::string name_;
    const std::string operation_;
    const std::string logHeader_;
    StatisticsOptions& options_;

    friend std::ostream& operator<<(std::ostream& os, const Operation& a) {
        a.print(os);
        return os;
    }
};


template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class OperationWithData : public Operation {
public:
    using Operation::name_;
    using Operation::options_;
    using Operation::logHeader_;

    OperationWithData(const std::string& name, const std::string& operation, long sz, StatisticsOptions& options) :
        Operation{name, operation, options}, values_{std::vector<T>(sz /= sizeof(T), 0.0)},count_{0} {}

    OperationWithData(const std::string& name, const std::string& operation, long sz, const std::string& partialPath, StatisticsOptions& options) :
        Operation{name, operation, options}, values_{std::vector<T>(sz /= sizeof(T), 0.0)},count_{0} {
            load(partialPath);
            return;
        }

    ~OperationWithData() = default;

    void reset( const void* data, long sz, eckit::DateTime dt  ) override {
        std::transform( values_.begin(), values_.end(), values_.begin(), [](T v){return static_cast<T>(0.0);} );
        count_ = 0;
        return;
    };

    void init( const void* data, long sz, eckit::DateTime dt ) override {
        // TODO: Used to save the first field of the window
        startTime_ = dt;
        return;
    };

    void init( eckit::DateTime dt ) override {
        // TODO: Used to save the initialization time of the window
        lastTime_ = dt;
        return;
    };    

    size_t byte_size() const override {
        return values_.size()*sizeof(T);
    };

    void dump(const std::string& partialPath, const long step) const {
        options_.dumper().startOperation( partialPath, name_, "w" );
        options_.dumper().writeOperation( count_, values_ );
        options_.dumper().endOperation();
    };

    void load(const std::string& partialPath) override {
        options_.reader().startOperation( partialPath, name_, "r" );
        options_.reader().readOperation( count_, values_ );
        options_.reader().endOperation();
    };

protected:

    void checkSize( long sz ){
        if (values_.size() != static_cast<size_t>(sz/sizeof(T))) {
            throw eckit::AssertionFailed(logHeader_ + " :: Expected size: " + std::to_string(values_.size())
                                         + " -- actual size: " + std::to_string(sz));
        }
    };
    void checkTimeInterval( ){
        long sec = count_ * options_.stepFreq() * options_.timeStep();
        if (sec == 0) {
            throw eckit::SeriousBug{logHeader_ + " :: Divide by zero", Here()};
        }
        return;
    };

    size_t count_;
    std::vector<T> values_;
    eckit::DateTime startTime_;
    eckit::DateTime lastTime_;

};

#include "operations/Accumulate.h"
#include "operations/Average.h"
#include "operations/FluxAverage.h"

#include "operations/Instant.h"
#include "operations/Minimum.h"
#include "operations/Maximum.h"

//==== Factory function ============================
template <typename T>
std::unique_ptr<Operation> make_operation(const std::string& opname, long sz, const std::string& partialPath,
                                             StatisticsOptions& options, bool restart) {

    if (opname == "instant") {
        return restart ? std::make_unique<Instant<T>>(opname, sz, partialPath, options)
                       : std::make_unique<Instant<T>>(opname, sz, options);
    }
    if (opname == "average") {
        return restart ? std::make_unique<Average<T>>(opname, sz, partialPath, options)
                       : std::make_unique<Average<T>>(opname, sz, options);
    }
    if (opname == "flux-average") {
        return restart ? std::make_unique<FluxAverage<T>>(opname, sz, partialPath, options)
                       : std::make_unique<FluxAverage<T>>(opname, sz, options);
    }
    if (opname == "minimum") {
        return restart ? std::make_unique<Minimum<T>>(opname, sz, partialPath, options)
                       : std::make_unique<Minimum<T>>(opname, sz, options);
    }
    if (opname == "maximum") {
        return restart ? std::make_unique<Maximum<T>>(opname, sz, partialPath, options)
                       : std::make_unique<Maximum<T>>(opname, sz, options);
    }
    if (opname != "accumulate") {
        std::ostringstream os;
        os << "Invalid opname in statistics operation :: " << opname << std::endl;
        throw eckit::UserError(os.str(), Here());
    }
    return restart ? std::make_unique<Accumulate<T>>(opname, sz, partialPath, options)
                   : std::make_unique<Accumulate<T>>(opname, sz, options);
}

}  // namespace action
