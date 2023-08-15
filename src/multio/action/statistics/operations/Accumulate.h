
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/StatisticsComponentsActivation.h"
#include "multio/action/statistics/operations/OperationWithData.h"
#include "multio/util/CastUtils.h"
#include "multio/util/ParallelPolicies.h"


#ifdef ENABLE_ACCUMULATE_OPERATION
namespace multio::action {

template <typename ComputationalType, typename InputOutputType,
          typename = std::enable_if_t<std::is_floating_point<InputOutputType>::value>>
class Accumulate final : public OperationWithData<ComputationalType, 1> {
public:
    using state = std::array<ComputationalType, 1>;
    using OperationWithData<ComputationalType, 1>::init;
    using OperationWithData<ComputationalType, 1>::byte_size;
    using OperationWithData<ComputationalType, 1>::needStepZero;
    using OperationWithData<ComputationalType, 1>::policy_;
    using OperationWithData<ComputationalType, 1>::profiler_;
    using OperationWithData<ComputationalType, 1>::name_;
    using OperationWithData<ComputationalType, 1>::cfg_;
    using OperationWithData<ComputationalType, 1>::logHeader_;
    using OperationWithData<ComputationalType, 1>::values_;
    using OperationWithData<ComputationalType, 1>::win_;
    using OperationWithData<ComputationalType, 1>::checkSize;
    using OperationWithData<ComputationalType, 1>::checkTimeInterval;


    Accumulate(const std::string& name, long sz, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<ComputationalType, 1>{name, "accumulate", sz / sizeof(InputOutputType), true, win, cfg} {}


    Accumulate(const std::string& name, long sz, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
               const StatisticsConfiguration& cfg) :
        OperationWithData<ComputationalType, 1>{name,      "accumulate", sz / sizeof(InputOutputType), true, win,
                                                IOmanager, cfg} {};


    size_t byte_size() const override final { return values_.size() * sizeof(InputOutputType); };


    bool needStepZero() const override { return false; };


    void init(const eckit::Buffer& data) override {
        profiler_[0].tic();
        checkSize(data.size() / sizeof(InputOutputType));
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const InputOutputType* val = static_cast<const InputOutputType*>(data.data());
        profiler_[0].toc();
        // TODO: Used to save the first field of the window
        return;
    };


    void updateData(const eckit::Buffer& data) override {
        profiler_[1].tic();
        checkSize(data.size() / sizeof(InputOutputType));
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const InputOutputType* val = static_cast<const InputOutputType*>(data.data());
        cfg_.haveMissingValue() ? updateWithMissing(val) : updateWithoutMissing(val);
        profiler_[1].toc();
        return;
    }


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
    }


private:
    void computeWithMissing(InputOutputType* buf) const {
        const InputOutputType m = static_cast<InputOutputType>(cfg_.missingValue());
        auto func = [m](const state& v) {
            InputOutputType v0 = util::staticCastValueMaybe<InputOutputType, ComputationalType>(v[0]);
            return ((m == v0) ? m : v0);
        };
        util::transform(policy_, values_.cbegin(), values_.cend(), buf, func);
        return;
    }


    void computeWithoutMissing(InputOutputType* buf) const {
        auto func = [](const state& v) { return util::staticCastValueMaybe<InputOutputType, ComputationalType>(v[0]); };
        util::transform(policy_, values_.cbegin(), values_.cend(), buf, func);
        return;
    }


    void updateWithoutMissing(const InputOutputType* val) {
        auto func = [](const state& v1, const InputOutputType& v2) {
            return state{v1[0] + util::staticCastValueMaybe<ComputationalType, InputOutputType>(v2)};
        };
        util::transform(policy_, values_.cbegin(), values_.cend(), val, values_.begin(), func);
        return;
    }


    void updateWithMissing(const InputOutputType* val) {
        const ComputationalType m = cfg_.missingValue();
        auto func = [m](const state& v1, const InputOutputType& v2) {
            ComputationalType v2_ = util::staticCastValueMaybe<ComputationalType, InputOutputType>(v2);
            return state{((m == v2_) ? m : v1[0] + v2_)};
        };
        util::transform(policy_, values_.cbegin(), values_.cend(), val, values_.begin(), func);
        return;
    }


    void updateWindow(const InputOutputType* val) {
        auto func = [](const state& v1, const InputOutputType& v2) {
            return state{util::staticCastValueMaybe<ComputationalType, InputOutputType>(0.0)};
        };
        util::transform(policy_, values_.begin(), values_.end(), val, values_.begin(), func);
        return;
    }


    void print(std::ostream& os) const override { os << logHeader_; }
};

}  // namespace multio::action

#endif
