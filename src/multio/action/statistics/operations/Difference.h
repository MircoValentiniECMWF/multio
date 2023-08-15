
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/StatisticsComponentsActivation.h"
#include "multio/action/statistics/operations/OperationWithData.h"
#include "multio/util/CastUtils.h"
#include "multio/util/ParallelPolicies.h"


#ifdef ENABLE_DIFFERENCE_OPERATION
namespace multio::action {

template <typename ComputationalType, typename InputOutputType,
          typename = std::enable_if_t<std::is_floating_point<InputOutputType>::value>>
class Difference final : public OperationWithData<ComputationalType, 2> {
public:
    using state = std::array<ComputationalType, 2>;
    using OperationWithData<ComputationalType, 2>::init;
    using OperationWithData<ComputationalType, 2>::byte_size;
    using OperationWithData<ComputationalType, 2>::needStepZero;
    using OperationWithData<ComputationalType, 2>::policy_;
    using OperationWithData<ComputationalType, 2>::profiler_;
    using OperationWithData<ComputationalType, 2>::name_;
    using OperationWithData<ComputationalType, 2>::cfg_;
    using OperationWithData<ComputationalType, 2>::logHeader_;
    using OperationWithData<ComputationalType, 2>::values_;
    using OperationWithData<ComputationalType, 2>::win_;
    using OperationWithData<ComputationalType, 2>::checkSize;
    using OperationWithData<ComputationalType, 2>::checkTimeInterval;


    Difference(const std::string& name, long sz, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<ComputationalType, 2>{name, "difference", sz / sizeof(InputOutputType), true, win, cfg} {}


    Difference(const std::string& name, long sz, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
               const StatisticsConfiguration& cfg) :
        OperationWithData<ComputationalType, 2>{name,      "difference", sz / sizeof(InputOutputType), true, win,
                                                IOmanager, cfg} {};


    size_t byte_size() const override final { return values_.size() * sizeof(InputOutputType); };


    bool needStepZero() const override { return true; };


    void init(const eckit::Buffer& data) override {
        profiler_[0].tic();
        checkSize(data.size() / sizeof(InputOutputType));
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const InputOutputType* val = static_cast<const InputOutputType*>(data.data());
        updateWindow(val);
        profiler_[0].toc();
        return;
    };


    void updateData(const eckit::Buffer& data) override {
        profiler_[1].tic();
        checkSize(data.size() / sizeof(InputOutputType));
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const InputOutputType* val = static_cast<const InputOutputType*>(data.data());
        updateData(val);
        profiler_[1].toc();
        return;
    };


    void updateWindow(const eckit::Buffer& data) override {
        profiler_[2].tic();
        checkSize(data.size() / sizeof(InputOutputType));
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const InputOutputType* val = static_cast<const InputOutputType*>(data.data());
        updateWindow(val);
        profiler_[2].toc();
        return;
    };


    void compute(eckit::Buffer& data) const override {
        profiler_[3].tic();
        checkSize(data.size() / sizeof(InputOutputType));
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
        auto func = [m](const state& v) {
            InputOutputType v0_ = util::staticCastValueMaybe<InputOutputType, ComputationalType>(v[0]);
            InputOutputType v1_ = util::staticCastValueMaybe<InputOutputType, ComputationalType>(v[1]);
            return ((m == v0_) ? m : v1_ - v0_);
        };
        util::transform(policy_, values_.cbegin(), values_.cend(), buf, func);
        return;
    }


    void computeWithoutMissing(InputOutputType* buf) const {
        auto func = [](const state& v) {
            InputOutputType v0_ = util::staticCastValueMaybe<InputOutputType, ComputationalType>(v[0]);
            InputOutputType v1_ = util::staticCastValueMaybe<InputOutputType, ComputationalType>(v[1]);
            return (v1_ - v0_);
        };
        util::transform(policy_, values_.cbegin(), values_.cend(), buf, func);
        return;
    }


    void updateData(const InputOutputType* val) {
        auto func = [](const state& v1, const InputOutputType& v2) {
            return state{v1[0], util::staticCastValueMaybe<ComputationalType, InputOutputType>(v2)};
        };
        util::transform(policy_, values_.begin(), values_.end(), val, values_.begin(), func);
        return;
    }


    void updateWindow(const InputOutputType* val) {
        auto func = [](const state& v1, const InputOutputType& v2) {
            return state{util::staticCastValueMaybe<ComputationalType, InputOutputType>(v2), util::staticCastValueMaybe<ComputationalType, InputOutputType>(0.0)};
        };
        util::transform(policy_, values_.begin(), values_.end(), val, values_.begin(), func);
        return;
    }


    void print(std::ostream& os) const override { os << logHeader_; }
};

}  // namespace multio::action

#endif