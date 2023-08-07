
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/StatisticsComponentsActivation.h"
#include "multio/action/statistics/operations/OperationWithData.h"

namespace multio::action {

#ifdef __ENABLE_VARIANCE_OPERATION__

template <typename ComputationalType, typename InputOutputType, class ExecutionPolicy, typename = std::enable_if_t<std::is_floating_point<InputOutputType>::value>>
class Variance : public OperationWithData<ComputationalType,ExecutionPolicy,2> {
public:


    using state = std::array<ComputationalType,2>;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::init;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::byte_size;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::profiler_;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::needStepZero;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::policy_;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::name_;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::cfg_;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::logHeader_;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::values_;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::win_;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::checkSize;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::checkTimeInterval;


    Variance(const std::string& name, long sz, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<ComputationalType,ExecutionPolicy,2>{name, "variance", sz, true, win, cfg} {}


    Variance(const std::string& name, long sz, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
             const StatisticsConfiguration& cfg) :
        OperationWithData<ComputationalType,ExecutionPolicy,2>{name, "variance", sz, true, win, IOmanager, cfg} {};


    size_t byte_size() const override final { return values_.size() * sizeof(InputOutputType); };


    bool needStepZero() const override { return false; };


    void init(const eckit::Buffer& data) override {
        profiler_[0].tic();
        checkSize(data.size());
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const InputOutputType* val = static_cast<const InputOutputType*>(data.data());
        profiler_[0].toc();
        // TODO: Used to save the first field of the window
        return;
    };


    void updateData(const eckit::Buffer& data) override {
        profiler_[1].tic();
        checkSize(data.size());
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const InputOutputType* val = static_cast<const InputOutputType*>(data.data());
        cfg_.haveMissingValue() ? updateWithMissing(val) : updateWithoutMissing(val);
        profiler_[1].toc();
        return;
    }


    void updateWindow(const eckit::Buffer& data) override {
        profiler_[2].tic();
        checkSize(data.size());
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const InputOutputType* val = static_cast<const InputOutputType*>(data.data());
        updateWindow( val );
        profiler_[2].toc();
        return;
    };


    void compute(eckit::Buffer& data) const override {
        profiler_[3].tic();
        checkSize(data.size());
        checkTimeInterval();
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        auto val = static_cast<InputOutputType*>(data.data());
        cfg_.haveMissingValue() ? computeWithMissing(val) : computeWithoutMissing(val);
        profiler_[3].toc();
        return;
    }


private:


    void updateWithoutMissing(const InputOutputType* val) {
        const ComputationalType c = static_cast<ComputationalType>(icntpp());
        std::transform(policy_, values_.begin(), values_.end(), val, values_.begin(), [c](const state& v1, const InputOutputType& v2) {
            state out;
            if constexpr ( std::is_same<InputOutputType,ComputationalType>::value ){
                out[0] = v1[0] + c * (v2 - v1[0]);
                out[1] = v1[1] + (v2 - v1[0]) * (v2 - out[0]);
            }
            else {
                ComputationalType v2_ = static_cast<ComputationalType>(v2);
                out[0] = v1[0] + c * (v2_ - v1[0]);
                out[1] = v1[1] + (v2_ - v1[0]) * (v2_ - out[0]);
            }
            return out;
        });
        return;
    }


    void updateWithMissing(const InputOutputType* val) {
        const ComputationalType c = static_cast<ComputationalType>(icntpp());
        const ComputationalType m = static_cast<ComputationalType>(cfg_.missingValue());
        std::transform(policy_, values_.cbegin(), values_.cend(), val, values_.begin(), [c, m](const state& v1, const InputOutputType& v2) {
            state out;
            if constexpr ( std::is_same<InputOutputType,ComputationalType>::value ){
                out[0] = m == v2 ? m : v1[0] + c * (v2 - v1[0]);
                out[1] = m == v2 ? m : v1[1] + (v2 - v1[0]) * (v2 - out[0]);
            }
            else {
                ComputationalType v2_ = static_cast<ComputationalType>(v2);
                out[0] = m == v2_ ? m : v1[0] + c * (v2_ - v1[0]);
                out[1] = m == v2_ ? m :  v1[1] + (v2_ - v1[0]) * (v2_ - out[0]);
            }
            return out;
        });
        return;
    }


    void updateWindow(const InputOutputType* val) {
        std::transform(policy_, values_.begin(), values_.end(), val, values_.begin(), [](const state& v1, const InputOutputType& v2) {
            return state{static_cast<ComputationalType>(0.0), static_cast<ComputationalType>(0.0)};
        });
        return;
    }


    void computeWithMissing(InputOutputType* buf) const {
        const InputOutputType c = static_cast<InputOutputType>(icntpp()); 
        const InputOutputType m = static_cast<InputOutputType>(cfg_.missingValue());
        std::transform(policy_, values_.cbegin(), values_.cend(), buf, [c, m](const state& v) { 
            if constexpr ( std::is_same<InputOutputType,ComputationalType>::value ){
                return ( (m == v[1]) ? m : v[1] * c);
            }
            else {
                InputOutputType v1_ = static_cast<InputOutputType>(v[1]);
                return ( (m == v1_) ? m : v1_ * c);
            }
        });
        return;
    }


    void computeWithoutMissing(InputOutputType* buf) const {
        const InputOutputType c = static_cast<InputOutputType>(icntpp()); 
        std::transform(policy_, values_.cbegin(), values_.cend(), buf, [c](const state& v) { 
            if constexpr ( std::is_same<InputOutputType,ComputationalType>::value ){
                return (v[1] * c);
            }
            else {
                InputOutputType v1_ = static_cast<InputOutputType>(v[1]);
                return (v1_ * c);
            }
        });
        return;
    }


    void print(std::ostream& os) const override { os << logHeader_; }


    double icntpp() const { return double(1.0) / double(win_.count()); };

};

}  // namespace multio::action

#endif