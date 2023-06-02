#pragma once

#include <cmath>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <variant>
#include <vector>

#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/io/Buffer.h"
#include "eckit/log/Log.h"

#include "multio/LibMultio.h"
#include "multio/Operation/statistics/StatisticsOptions.h"
#include "multio/util/VariantHelpers.h"

namespace multio {
namespace Operation {

//==== Base class =================================

template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class Operation {
public:
    Operation(const std::string& name, const std::string& operation, long sz, const StatisticsOptions& options) :
        name_{name}, operation_{operation}, values_{std::vector<T>(sz /= sizeof(T), 0.0)}, options_{options} {}
    const std::string& name() { return name_; }
    const std::string& operation() { return operation_; }

    virtual eckit::Buffer compute() = 0;
    virtual void update(const void* val, long sz) = 0;

    virtual ~Operation() = default;
    virtual void dump(const std::string& partialPath, const long step) const = 0;

protected:
    virtual void print(std::ostream& os) const = 0;

    std::string name_;
    std::string operation_;
    std::vector<T> values_;
    const StatisticsOptions& options_;

    bool isInsideTolerance(T val) const { return (static_cast<double>(val) == options_.missingValue()); };

    friend std::ostream& operator<<(std::ostream& os, const Operation& a) {
        a.print(os);
        return os;
    }
};


using OperationVar = std::variant<std::unique_ptr<Operation<double>>, std::unique_ptr<Operation<float>>>;


//==== Derived classes ============================
template <typename T>
class Instant final : public Operation<T> {
public:
    using Operation<T>::values_;
    using Operation<T>::options_;

    Instant(const std::string& name, long sz, const StatisticsOptions& options) :
        Operation<T>{name, "instant", sz, options} {}

    Instant(const std::string& name, long sz, const std::string& partialPath, const StatisticsOptions& options) :
        Operation<T>{name, "instant", sz, options} {

        return;
    };

    void dump(const std::string& partialPath, const long step) const override {}

    eckit::Buffer compute() override { return eckit::Buffer{values_.data(), values_.size() * sizeof(T)}; }

    void update(const void* data, long sz) override {
        auto val = static_cast<const T*>(data);
        sz /= sizeof(T);

        ASSERT(values_.size() == static_cast<size_t>(sz));

        // May never be needed -- just creates an unnecessarily copy
        std::copy(val, val + sz, values_.begin());
    }

private:
    void print(std::ostream& os) const override { os << "Operation(instant)"; }
};


template <typename T>
class Average final : public Operation<T> {
    long count_ = 0;

public:
    using Operation<T>::name_;
    using Operation<T>::values_;
    using Operation<T>::options_;
    using Operation<T>::isInsideTolerance;

    Average(const std::string& name, long sz, const StatisticsOptions& options) :
        Operation<T>{name, "average", sz, options} {}

    Average(const std::string& name, long sz, const std::string& partialPath, const StatisticsOptions& options) :
        Operation<T>{name, "average", sz, options} {
        // std::ostringstream tmpOs;
        std::ostringstream defOs;
        // tmpOs << partialPath << "-average.tmp.bin";
        defOs << partialPath << "-" << options.restartStep() << "-average.bin";
        // eckit::PathName tmpFile(tmpOs.str());
        // eckit::PathName defFile(defOs.str());
        std::string fname = defOs.str();
        std::ifstream wf(fname, std::ios::binary);
        if (!wf) {
            throw eckit::SeriousBug("Cannot open file!", Here());
        }
        long dim;
        wf.read((char*)&count_, sizeof(long));
        wf.read((char*)&dim, sizeof(long));
        long checksum = 0;
        long cs = 0;
        checksum ^= count_;
        checksum ^= dim;
        if (dim != sz / sizeof(T)) {
            std::ostringstream err;
            err << "Wrong size during restart of average-statistics :: (" << fname << ") " << dim << ", " << sz;
            throw eckit::SeriousBug(err.str(), Here());
        }
        LOG_DEBUG_LIB(LibMultio) << "The counter is :: " << count_ << std::endl;
        values_.resize(dim);
        for (int i = 0; i < dim; ++i) {
            double tmp;
            wf.read((char*)&tmp, sizeof(double));
            checksum ^= *((long*)&tmp);
            values_[i] = static_cast<T>(tmp);
        }
        wf.read((char*)&cs, sizeof(long));
        wf.close();
        if (!wf.good()) {
            std::ostringstream err;
            err << "Error occurred at writing time :: " << fname;
            throw eckit::SeriousBug(err.str(), Here());
        }
        if (cs != checksum) {
            std::ostringstream err;
            err << "Error checksum not correct :: (" << fname << ") " << cs << ", " << checksum;
            throw eckit::SeriousBug(err.str(), Here());
        }
        return;
    };

    void dump(const std::string& partialPath, const long step) const override {
        std::ostringstream tmpOs;
        std::ostringstream defOs;
        std::ostringstream oldOs;
        tmpOs << partialPath << "-" << std::to_string(step) << "-average.tmp.bin";
        defOs << partialPath << "-" << std::to_string(step) << "-average.bin";
        oldOs << partialPath << "-" << std::to_string(step - 2) << "-average.bin";
        if (eckit::PathName oldFile(oldOs.str()); oldFile.exists()) {
            oldFile.unlink();
        }
        eckit::PathName tmpFile(tmpOs.str());
        eckit::PathName defFile(defOs.str());
        std::string fname = tmpOs.str();
        LOG_DEBUG_LIB(LibMultio) << fname << " - " << values_.size() << std::endl;
        std::ofstream wf(fname, std::ios::binary | std::ofstream::trunc);
        if (!wf) {
            std::ostringstream err;
            err << "Cannot open file :: (" << fname << ")";
            throw eckit::SeriousBug(err.str(), Here());
        }
        long sz = values_.size();
        long checksum = 0;
        wf.write((char*)&count_, sizeof(long));
        if (!wf.good()) {
            std::ostringstream err;
            err << "Error writing counter! (" << fname << ")";
            throw eckit::SeriousBug(err.str(), Here());
        }
        wf.write((char*)&sz, sizeof(long));
        if (!wf.good()) {
            std::ostringstream err;
            err << "Error writing size! (" << fname << ")";
            throw eckit::SeriousBug(err.str(), Here());
        }
        checksum ^= count_;
        checksum ^= sz;
        for (int i = 0; i < sz; ++i) {
            double tmp = double(values_[i]);
            checksum ^= *((long*)&tmp);
            wf.write((char*)&tmp, sizeof(double));
            if (!wf.good()) {
                std::ostringstream err;
                err << "Error writing data! (" << fname << ") " << i;
                throw eckit::SeriousBug(err.str(), Here());
            }
        }
        wf.write((char*)&checksum, sizeof(long));
        if (!wf.good()) {
            std::ostringstream err;
            err << "Error setting checksum! (" << fname << ")";
            throw eckit::SeriousBug(err.str(), Here());
        }
        wf.close();
        if (!wf.good()) {
            std::ostringstream err;
            err << "Error occurred at writing time! (" << fname << ")";
            throw eckit::SeriousBug(err.str(), Here());
        }
        eckit::PathName::rename(tmpFile, defFile);
        return;
    }


    eckit::Buffer compute() override {
        LOG_DEBUG_LIB(LibMultio) << "statistics (" << name_ << ") compute :: count=" << count_ << std::endl;
        return eckit::Buffer{values_.data(), values_.size() * sizeof(T)};
    }

    void update(const void* data, long sz) override {
        auto val = static_cast<const T*>(data);
        sz /= sizeof(T);

        if (values_.size() != static_cast<size_t>(sz)) {
            throw eckit::AssertionFailed("Expected size: " + std::to_string(values_.size())
                                         + " -- actual size: " + std::to_string(sz));
        }

        try {
            // Compute the running average in order to avoid precison problems
            // TODO: the scale factor can be computed using eckit::frOperation
            // TODO: Handling missing values
            double icntpp = double(1.0) / double(count_ + 1);
            double sc = double(count_) * icntpp;
            if (options_.haveMissingValue()) {
                for (int i = 0; i < sz; ++i) {
                    values_[i] = isInsideTolerance(val[i]) ? static_cast<T>(options_.missingValue())
                                                           : values_[i] * sc + (val[i]) * icntpp;
                }
            }
            else {
                for (auto& v : values_) {
                    v = v * sc + (*val++) * icntpp;
                }
            }
            ++count_;
        }
        catch (...) {
            LOG_DEBUG_LIB(LibMultio) << "ACCUMULATED DATA" << std::endl;
            for (auto& v : values_) {
                LOG_DEBUG_LIB(LibMultio) << v << ", ";
            }
            LOG_DEBUG_LIB(LibMultio) << std::endl;
            LOG_DEBUG_LIB(LibMultio) << "NEW DATA" << std::endl;
            val = static_cast<const T*>(data);
            for (auto& v : values_) {
                LOG_DEBUG_LIB(LibMultio) << val++ << ", ";
            }
            LOG_DEBUG_LIB(LibMultio) << std::endl;
            throw eckit::SeriousBug("numerical error during average update", Here());
        }
    }

private:
    void print(std::ostream& os) const override { os << "Operation(average)"; }
};

template <typename T>
class FluxAverage final : public Operation<T> {
    long count_ = 0;

public:
    using Operation<T>::name_;
    using Operation<T>::values_;
    using Operation<T>::options_;
    using Operation<T>::isInsideTolerance;

    FluxAverage(const std::string& name, long sz, const StatisticsOptions& options) :
        Operation<T>{name, "average", sz, options} {}

    FluxAverage(const std::string& name, long sz, const std::string& partialPath, const StatisticsOptions& options) :
        Operation<T>{name, "average", sz, options} {
        std::ostringstream os;
        os << partialPath << "-" << options.restartStep() << "-flux-average.bin";
        std::string fname = os.str();
        std::ifstream wf(fname, std::ios::binary);
        if (!wf) {
            throw eckit::SeriousBug("Cannot open file!", Here());
        }
        long dim;
        wf.read((char*)&count_, sizeof(long));
        wf.read((char*)&dim, sizeof(long));
        long checksum = 0;
        long cs = 0;
        checksum ^= count_;
        checksum ^= dim;
        if (dim != sz / sizeof(T)) {
            std::ostringstream err;
            err << "Wrong size during restart of average-statistics :: " << dim << ", " << sz;
            throw eckit::SeriousBug(err.str(), Here());
        }
        LOG_DEBUG_LIB(LibMultio) << "The counter is :: " << count_ << std::endl;
        values_.resize(dim);
        for (int i = 0; i < dim; ++i) {
            double tmp;
            wf.read((char*)&tmp, sizeof(double));
            checksum ^= static_cast<long>(tmp);
            values_[i] = static_cast<T>(tmp);
        }
        wf.read((char*)&cs, sizeof(long));
        wf.close();
        if (!wf.good()) {
            std::ostringstream err;
            err << "Error occurred at writing time :: " << fname;
            throw eckit::SeriousBug(err.str(), Here());
        }
        if (cs != checksum) {
            std::ostringstream err;
            err << "Error checksum not correct :: " << cs << ", " << checksum;
            throw eckit::SeriousBug(err.str(), Here());
        }
        return;
    };

    void dump(const std::string& partialPath, const long step) const override {
        std::ostringstream tmpOs;
        std::ostringstream defOs;
        std::ostringstream oldOs;
        tmpOs << partialPath << "-" << std::to_string(step) << "-flux-average.tmp.bin";
        defOs << partialPath << "-" << std::to_string(step) << "-flux-average.bin";
        oldOs << partialPath << "-" << std::to_string(step - 2) << "-flux-average.bin";
        if (eckit::PathName oldFile(oldOs.str()); oldFile.exists()) {
            oldFile.unlink();
        }
        eckit::PathName tmpFile(tmpOs.str());
        eckit::PathName defFile(defOs.str());
        std::string fname = tmpOs.str();
        std::ofstream wf(fname, std::ios::binary | std::ofstream::trunc);
        if (!wf) {
            throw eckit::SeriousBug("Cannot open file!", Here());
        }
        long sz = values_.size();
        long checksum = 0;
        wf.write((char*)&count_, sizeof(long));
        wf.write((char*)&sz, sizeof(long));
        checksum ^= count_;
        checksum ^= sz;
        for (int i = 0; i < sz; ++i) {
            double tmp = double(values_[i]);
            checksum ^= static_cast<long>(tmp);
            wf.write((char*)&tmp, sizeof(double));
        }
        wf.write((char*)&checksum, sizeof(long));
        wf.close();
        if (!wf.good()) {
            throw eckit::SeriousBug("Error occurred at writing time!", Here());
        }
        eckit::PathName::rename(tmpFile, defFile);
    }


    eckit::Buffer compute() override {

        if (options_.haveMissingValue()) {
            long sec = count_ * options_.stepFreq() * options_.timeStep();
            if (sec == 0) {
                throw eckit::SeriousBug{"Divide by zero", Here()};
            }
            for (int i = 0; i < values_.size(); ++i) {
                // TODO: Need to understand if this case is possible
                values_[i] = isInsideTolerance(values_[i]) ? static_cast<T>(options_.missingValue())
                                                           : values_[i] / static_cast<T>(sec);
            }
        }
        else {
            for (auto& val : values_) {
                val /= static_cast<T>(count_ * options_.stepFreq() * options_.timeStep());
            }
        }
        LOG_DEBUG_LIB(LibMultio) << "statistics (" << name_ << ") compute :: count=" << count_ << std::endl;
        return eckit::Buffer{values_.data(), values_.size() * sizeof(T)};
    }

    void update(const void* data, long sz) override {
        auto val = static_cast<const T*>(data);
        sz /= sizeof(T);
        if (values_.size() != static_cast<size_t>(sz)) {
            throw eckit::AssertionFailed("Expected size: " + std::to_string(values_.size())
                                         + " -- actual size: " + std::to_string(sz));
        }
        // May never be needed -- just creates an unnecessarily copy
        std::copy(val, val + sz, values_.begin());
        ++count_;
    }

private:
    void print(std::ostream& os) const override { os << "Operation(flux-average)"; }
};


template <typename T>
class Minimum final : public Operation<T> {
public:
    using Operation<T>::values_;
    using Operation<T>::options_;

    Minimum(const std::string& name, long sz, const StatisticsOptions& options) :
        Operation<T>{name, "minimum", sz, options} {}

    Minimum(const std::string& name, long sz, const std::string& partialPath, const StatisticsOptions& options) :
        Operation<T>{name, "minimum", sz, options} {
        return;
    };

    void dump(const std::string& partialPath, const long step) const override {}


    eckit::Buffer compute() override { return eckit::Buffer{values_.data(), values_.size() * sizeof(T)}; }

    void update(const void* data, long sz) override {
        auto val = static_cast<const T*>(data);
        sz /= sizeof(T);

        ASSERT(values_.size() == static_cast<size_t>(sz));

        for (auto& v : values_) {
            v = (v > *val) ? *val : v;
            ++val;
        }
    }

private:
    void print(std::ostream& os) const override { os << "Operation(minimum)"; }
};


template <typename T>
class Maximum final : public Operation<T> {
public:
    using Operation<T>::values_;
    using Operation<T>::options_;

    Maximum(const std::string& name, long sz, const StatisticsOptions& options) :
        Operation<T>{name, "maximum", sz, options} {}

    Maximum(const std::string& name, long sz, const std::string& partialPath, const StatisticsOptions& options) :
        Operation<T>{name, "maximum", sz, options} {
        return;
    };

    void dump(const std::string& partialPath, const long step) const override {}


    eckit::Buffer compute() override { return eckit::Buffer{values_.data(), values_.size() * sizeof(T)}; }

    void update(const void* data, long sz) override {
        auto val = static_cast<const T*>(data);
        sz /= sizeof(T);

        ASSERT(values_.size() == static_cast<size_t>(sz));

        for (auto& v : values_) {
            v = (v < *val) ? *val : v;
            ++val;
        }
    }


private:
    void print(std::ostream& os) const override { os << "Operation(maximum)"; }
};

template <typename T>
class Accumulate final : public Operation<T> {
public:
    using Operation<T>::values_;
    using Operation<T>::options_;
    using Operation<T>::isInsideTolerance;

    Accumulate(const std::string& name, long sz, const StatisticsOptions& options) :
        Operation<T>{name, "accumulate", sz, options} {};

    Accumulate(const std::string& name, long sz, const std::string& partialPath, const StatisticsOptions& options) :
        Operation<T>{name, "accumulate", sz, options} {
        std::ostringstream defOs;
        // tmpOs << partialPath << "-average.tmp.bin";
        defOs << partialPath << "-" << options.restartStep() << "-accumulate.bin";
        // eckit::PathName tmpFile(tmpOs.str());
        // eckit::PathName defFile(defOs.str());
        std::string fname = defOs.str();
        std::ifstream wf(fname, std::ios::binary);
        if (!wf) {
            throw eckit::SeriousBug("Cannot open file!", Here());
        }
        long dim;
        wf.read((char*)&dim, sizeof(long));
        long checksum = 0;
        long cs = 0;
        checksum ^= dim;
        if (dim != sz / sizeof(T)) {
            std::ostringstream err;
            err << "Wrong size during restart of average-statistics :: " << dim << ", " << sz;
            throw eckit::SeriousBug(err.str(), Here());
        }
        values_.resize(dim);
        for (int i = 0; i < dim; ++i) {
            double tmp;
            wf.read((char*)&tmp, sizeof(double));
            checksum ^= *((long*)&tmp);
            values_[i] = static_cast<T>(tmp);
        }
        wf.read((char*)&cs, sizeof(long));
        wf.close();
        if (!wf.good()) {
            std::ostringstream err;
            err << "Error occurred at writing time :: " << fname;
            throw eckit::SeriousBug(err.str(), Here());
        }
        if (cs != checksum) {
            std::ostringstream err;
            err << "Error checksum not correct :: " << cs << ", " << checksum;
            throw eckit::SeriousBug(err.str(), Here());
        }
        return;
    };

    void dump(const std::string& partialPath, const long step) const override {
        std::ostringstream tmpOs;
        std::ostringstream defOs;
        std::ostringstream oldOs;
        tmpOs << partialPath << "-" << std::to_string(step) << "-accumulate.tmp.bin";
        defOs << partialPath << "-" << std::to_string(step) << "-accumulate.bin";
        oldOs << partialPath << "-" << std::to_string(step - 2) << "-accumulate.bin";
        if (eckit::PathName oldFile(oldOs.str()); oldFile.exists()) {
            oldFile.unlink();
        }
        eckit::PathName tmpFile(tmpOs.str());
        eckit::PathName defFile(defOs.str());
        std::string fname = tmpOs.str();
        LOG_DEBUG_LIB(LibMultio) << fname << " - " << values_.size() << std::endl;
        std::ofstream wf(fname, std::ios::binary | std::ofstream::trunc);
        if (!wf) {
            throw eckit::SeriousBug("Cannot open file!", Here());
        }
        long sz = values_.size();
        long checksum = 0;
        wf.write((char*)&sz, sizeof(long));
        checksum ^= sz;
        for (int i = 0; i < sz; ++i) {
            double tmp = double(values_[i]);
            checksum ^= *((long*)&tmp);
            wf.write((char*)&tmp, sizeof(double));
        }
        wf.write((char*)&checksum, sizeof(long));
        wf.close();
        if (!wf.good()) {
            throw eckit::SeriousBug("Error occurred at writing time!", Here());
        }
        eckit::PathName::rename(tmpFile, defFile);
        return;
    }


    eckit::Buffer compute() override { return eckit::Buffer{values_.data(), values_.size() * sizeof(T)}; }

    void update(const void* data, long sz) override {
        auto val = static_cast<const T*>(data);
        sz /= sizeof(T);

        ASSERT(values_.size() == static_cast<size_t>(sz));

        if (options_.haveMissingValue()) {
            for (int i = 0; i < sz; ++i) {
                values_[i] = isInsideTolerance(val[i]) ? static_cast<T>(options_.missingValue()) : values_[i] + val[i];
            }
        }
        else {
            for (auto& v : values_) {
                v += *val++;
            }
        }
    }

private:
    void print(std::ostream& os) const override { os << "Operation(accumulate)"; }
};

//==== Factory function ============================
template <typename T>
std::unique_ptr<Operation<T>> make_operation(const std::string& opname, long sz, const std::string& partialPath,
                                             const StatisticsOptions& options, bool restart) {

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



class OperationBuilderBase;

class OperationFactory : private eckit::NonCopyable {
private:  // methods
    OperationFactory() {}

public:  // methods
    static OperationFactory& instance();

    void enregister(const std::string& name, const OperationBuilderBase* builder);
    void deregister(const std::string& name);

    void list(std::ostream&);

    std::unique_ptr<Operation> build(const std::string&, const ConfigurationContext& confCtx);

private:  // members
    std::map<std::string, const OperationBuilderBase*> factories_;

    std::recursive_mutex mutex_;
};

class OperationBuilderBase : private eckit::NonCopyable {
public:  // methods
    virtual std::unique_ptr<Operation> make(const ConfigurationContext& confCtx) const = 0;

protected:  // methods
    OperationBuilderBase(const std::string&);

    virtual ~OperationBuilderBase();

    std::string name_;
};

template <typename dataType, class T<dataType> >
class OperationBuilder final : public OperationBuilderBase {
    std::unique_ptr<Operation> make(const ConfigurationContext& confCtx) const override { return std::make_unique<datatype,T<dataType>>(confCtx); }

public:
    OperationBuilder(const std::string& name) : OperationBuilderBase(name) {}
};


}  // namespace Operation
}  // namespace multio
