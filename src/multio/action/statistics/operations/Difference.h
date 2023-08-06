
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/OperationActivation.h"
#include "multio/action/statistics/operations/OperationWithData.h"

#ifdef __ENABLE_DIFFERENCE_OPERATION__
namespace multio::action {

template <typename ComputationalType, typename InputOutputType, class ExecutionPolicy, typename = std::enable_if_t<std::is_floating_point<InputOutputType>::value>>
class Difference final : public OperationWithData<ComputationalType,ExecutionPolicy,2> {
public:


    using state = std::array<ComputationalType,2>;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::init;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::byte_size;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::needStepZero;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::policy_;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::profiler_;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::name_;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::cfg_;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::logHeader_;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::values_;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::win_;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::checkSize;
    using OperationWithData<ComputationalType,ExecutionPolicy,2>::checkTimeInterval;


    Difference(const std::string& name, long sz, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<ComputationalType,ExecutionPolicy,2>{name, "difference", sz, true, win, cfg} {}


    Difference(const std::string& name, long sz, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
               const StatisticsConfiguration& cfg) :
        OperationWithData<ComputationalType,ExecutionPolicy,2>{name, "difference", sz, true, win, IOmanager, cfg} {};


    size_t byte_size() const override final { return values_.size() * sizeof(ComputationalType); };


    bool needStepZero() const override { return true; };


    void init(const eckit::Buffer& data) override {
        profiler_[0].tic();
        checkSize(data.size());
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const InputOutputType* val = static_cast<const InputOutputType*>(data.data());
        updateWindow(val);
        profiler_[0].toc();
        return;
    };


    void updateData(const eckit::Buffer& data) override {
        profiler_[1].tic();
        checkSize(data.size());
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const InputOutputType* val = static_cast<const InputOutputType*>(data.data());
        updateData(val);
        profiler_[1].toc();
        return;
    };


    void updateWindow(const eckit::Buffer& data) override {
        profiler_[2].tic();
        checkSize(data.size());
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const InputOutputType* val = static_cast<const InputOutputType*>(data.data());
        updateWindow(val);
        profiler_[2].toc();
        return;
    };


    void computeData(eckit::Buffer& data) const override {
        profiler_[3].tic();
        checkSize(data.size());
        checkTimeInterval();
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        InputOutputType* val = static_cast<InputOutputType*>(data.data());
        cfg_.haveMissingValue() ? computeWithMissing(val) : computeWithoutMissing(val);
        profiler_[3].toc();
        return;
    };


private:


    void computeWithMissing(InputOutputType* buf) const {
        const InputOutputType m = static_cast<InputOutputType>(cfg_.missingValue());
        std::transform(policy_, values_.cbegin(), values_.cend(), buf, [m](const state& v) {
            if constexpr ( std::is_same<InputOutputType,ComputationalType>::value ){
                return ((m==v[0]) ? m : v[1] - v[0]);
            }            
            else {            
                InputOutputType v0_ = static_cast<InputOutputType>(v[0]);
                InputOutputType v1_ = static_cast<InputOutputType>(v[1]);
                return ((m==v0_) ? m : v1_ - v0_);
            }
        });
        return;
    }


    void computeWithoutMissing(InputOutputType* buf) const {
        std::transform(policy_, values_.cbegin(), values_.cend(), buf, [](const state& v) { 
            if constexpr ( std::is_same<InputOutputType,ComputationalType>::value ){
                return (v[1] - v[0]);
            }            
            else {            
                InputOutputType v0_ = static_cast<InputOutputType>(v[0]);
                InputOutputType v1_ = static_cast<InputOutputType>(v[1]);
                return (v1_-v0_);
            }
        });
        return;
    }


    void updateData(const InputOutputType* val) {
        std::transform(policy_, values_.begin(), values_.end(), val, values_.begin(), [](const state& v1, const InputOutputType& v2) {
            if constexpr ( std::is_same<InputOutputType,ComputationalType>::value ){
                return state{v1[0], v2};
            }            
            else {
                return state{v1[0], static_cast<ComputationalType>(v2)};
            }
        });
        return;
    }


    void updateWindow(const InputOutputType* val) {
        std::transform(policy_, values_.begin(), values_.end(), val, values_.begin(), [](const state& v1, const InputOutputType& v2) {
            if constexpr ( std::is_same<InputOutputType,ComputationalType>::value ){
                return state{v2,static_cast<ComputationalType>(0.0)};
            }            
            else {
                return state{static_cast<ComputationalType>(v2),static_cast<ComputationalType>(0.0)};
            }
        });
        return;
    }


    void print(std::ostream& os) const override { os << logHeader_; }

};

}  // namespace multio::action

#endif