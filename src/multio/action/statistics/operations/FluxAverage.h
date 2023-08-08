
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/StatisticsComponentsActivation.h"
#include "multio/action/statistics/operations/OperationWithData.h"

#ifdef __ENABLE_FLUXAVERAGE_OPERATION__
namespace multio::action {

template <typename ComputationalType, typename InputOutputType, class ExecutionPolicy, typename = std::enable_if_t<std::is_floating_point<InputOutputType>::value>>
class FluxAverage final : public OperationWithData<ComputationalType,ExecutionPolicy,1> {
public:


    using state = std::array<ComputationalType,1>;
    using OperationWithData<ComputationalType,ExecutionPolicy,1>::init;
    using OperationWithData<ComputationalType,ExecutionPolicy,1>::byte_size;
    using OperationWithData<ComputationalType,ExecutionPolicy,1>::needStepZero;
    using OperationWithData<ComputationalType,ExecutionPolicy,1>::policy_;
    using OperationWithData<ComputationalType,ExecutionPolicy,1>::profiler_;
    using OperationWithData<ComputationalType,ExecutionPolicy,1>::name_;
    using OperationWithData<ComputationalType,ExecutionPolicy,1>::cfg_;
    using OperationWithData<ComputationalType,ExecutionPolicy,1>::logHeader_;
    using OperationWithData<ComputationalType,ExecutionPolicy,1>::values_;
    using OperationWithData<ComputationalType,ExecutionPolicy,1>::win_;
    using OperationWithData<ComputationalType,ExecutionPolicy,1>::checkSize;
    using OperationWithData<ComputationalType,ExecutionPolicy,1>::checkTimeInterval;


    FluxAverage(const std::string& name, long sz, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<ComputationalType,ExecutionPolicy,1>{name, "average", sz/sizeof(InputOutputType), true, win, cfg} {}


    FluxAverage(const std::string& name, long sz, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
                const StatisticsConfiguration& cfg) :
        OperationWithData<ComputationalType,ExecutionPolicy,1>{name, "average", sz/sizeof(InputOutputType), true, win, IOmanager, cfg} {};


    size_t byte_size() const override final { return values_.size() * sizeof(InputOutputType); };


    bool needStepZero() const override { return false; };


    void init(const eckit::Buffer& data) override {
        profiler_[0].tic();
        checkSize(data.size()/sizeof(InputOutputType));
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const InputOutputType* val = static_cast<const InputOutputType*>(data.data());
        profiler_[0].toc();
        // TODO: Used to save the first field of the window
        return;
    };


    void updateWindow(const eckit::Buffer& data) override {
        profiler_[1].tic();
        checkSize(data.size()/sizeof(InputOutputType));
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const InputOutputType* val = static_cast<const InputOutputType*>(data.data());
        updateWindow(val);
        profiler_[1].toc();
        return;
    };


    void updateData(const eckit::Buffer& data) override {
        profiler_[2].tic();
        checkSize(data.size()/sizeof(InputOutputType));
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const InputOutputType* val = static_cast<const InputOutputType*>(data.data());
        update(val);
        profiler_[2].toc();
        return;
    }


    void compute(eckit::Buffer& data) const override {
        profiler_[3].tic();
        checkSize(data.size()/sizeof(InputOutputType));
        checkTimeInterval();
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        InputOutputType* val = static_cast<InputOutputType*>(data.data());
        cfg_.haveMissingValue() ? computeWithMissing(val) : computeWithoutMissing(val);
        profiler_[3].toc();
        return;
    }


private:


    void computeWithMissing(InputOutputType* buf) const {
        const InputOutputType m = static_cast<InputOutputType>(cfg_.missingValue());
        const InputOutputType c = static_cast<InputOutputType>(icntpp());
        std::transform(policy_, values_.cbegin(), values_.cend(), buf, [c, m](const state& v) {
            if constexpr (std::is_same<InputOutputType,ComputationalType>::value ){
                return ((m==v[0]) ? m : v[0]*c);
            }            
            else {            
                InputOutputType v0_ = static_cast<InputOutputType>(v[0]);
                return ((m==v0_) ? m : v0_*c);
            }        
        });
        return;
    }


    void computeWithoutMissing(InputOutputType* buf) const {
        const InputOutputType c = static_cast<InputOutputType>(icntpp());
        std::transform(policy_, values_.cbegin(), values_.cend(), buf, [c](const state& v) {
            if constexpr (std::is_same<InputOutputType,ComputationalType>::value ){
                return v[0]*c;
            }            
            else {            
                InputOutputType v0_ = static_cast<InputOutputType>(v[0]);
                return v0_*c;
            }
        });
        return;
    }


    void update(const InputOutputType* val) {
        std::transform(policy_, values_.cbegin(), values_.cend(), val, values_.begin(), [](const state& v1, const InputOutputType& v2) { 
            if constexpr (std::is_same<InputOutputType,ComputationalType>::value ){
                return state{v2};
            }            
            else {            
                return state{static_cast<ComputationalType>(v2)};
            } 
        });
        return;
    }


    void updateWindow(const InputOutputType* val) {
        std::transform(policy_, values_.begin(), values_.end(), val, values_.begin(), [](const state& v1, const InputOutputType& v2) {
            return state{static_cast<ComputationalType>(0.0)};
        });
        return;
    }


    void print(std::ostream& os) const override { os << logHeader_; }


    double icntpp() const { return static_cast<double>(1.0) / static_cast<double>(win_.count() * cfg_.stepFreq() * cfg_.timeStep()); };    

};

}  // namespace multio::action

#endif