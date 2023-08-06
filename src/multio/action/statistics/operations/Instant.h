
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/OperationActivation.h"
#include "multio/action/statistics/operations/OperationWithData.h"


#ifdef __ENABLE_INSTANT_OPERATION__
namespace multio::action {

template <typename ComputationalType, typename InputOutputType, class ExecutionPolicy,  typename = std::enable_if_t<std::is_floating_point<InputOutputType>::value>>
class Instant final : public OperationWithData<ComputationalType,ExecutionPolicy,1> {
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


    Instant(const std::string& name, long sz, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<ComputationalType,ExecutionPolicy,1>{name, "instant", sz, true, win, cfg} {}


    Instant(const std::string& name, long sz, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
            const StatisticsConfiguration& cfg) :
        OperationWithData<ComputationalType,ExecutionPolicy,1>{name, "instant", sz, true, win, IOmanager, cfg} {};


    size_t byte_size() const override final { return values_.size() * sizeof(ComputationalType); };


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
        update(val);
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


    void compute(eckit::Buffer& data) const override {
        profiler_[3].tic();
        checkSize(data.size());
        checkTimeInterval();
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        InputOutputType* val = static_cast<InputOutputType*>(data.data());
        compute(val);
        profiler_[3].toc();
        return;
    };


private:


    void compute(InputOutputType* buf) const {
        std::transform(policy_, values_.cbegin(), values_.cend(), buf, [](const state& v) { 
            if constexpr (std::is_same<InputOutputType,ComputationalType>::value ){
                return v[0];
            }
            else {
                return static_cast<InputOutputType>(v[0]);
            }
        } );   
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


};
}  // namespace multio::action

#endif