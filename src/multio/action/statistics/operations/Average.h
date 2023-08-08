
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/StatisticsComponentsActivation.h"
#include "multio/action/statistics/operations/OperationWithData.h"


#ifdef __ENABLE_AVERAGE_OPERATION__

namespace multio::action {

template <typename ComputationalType, typename InputOutputType,
          typename = std::enable_if_t<std::is_floating_point<InputOutputType>::value>>
class Average final : public OperationWithData<ComputationalType, 1> {
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


    Average(const std::string& name, long sz, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<ComputationalType, 1>{name, "average", sz / sizeof(InputOutputType), true, win, cfg} {}


    Average(const std::string& name, long sz, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
            const StatisticsConfiguration& cfg) :
        OperationWithData<ComputationalType, 1>{name,      "average", sz / sizeof(InputOutputType), true, win,
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
        cfg_.haveMissingValue() ? updateDataWithMissing(val) : updateDataWithoutMissing(val);
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
        compute(val);
        profiler_[3].toc();
        return;
    }

private:
    void compute(InputOutputType* buf) const {
        auto func = [](const state& v) {
            if constexpr (std::is_same<InputOutputType, ComputationalType>::value) {
                return v[0];
            }
            else {
                return static_cast<InputOutputType>(v[0]);
            }
        };
        util::transform(policy_, values_.cbegin(), values_.cend(), buf, func);
        return;
    }


    void updateDataWithoutMissing(const InputOutputType* val) {
        const ComputationalType c = icntpp();
        auto func = [c](const state& v1, const InputOutputType& v2) {
            if constexpr (std::is_same<InputOutputType, ComputationalType>::value) {
                return state{v1[0] + c * (v2 - v1[0])};
            }
            else {
                return state{v1[0] + c * (static_cast<ComputationalType>(v2) - v1[0])};
            }
        };
        util::transform(policy_, values_.cbegin(), values_.cend(), val, values_.begin(), func);
        return;
    }


    void updateDataWithMissing(const InputOutputType* val) {
        const ComputationalType c = icntpp();
        const ComputationalType m = static_cast<ComputationalType>(cfg_.missingValue());
        auto func = [c, m](const state& v1, const InputOutputType& v2) {
            if constexpr (std::is_same<InputOutputType, ComputationalType>::value) {
                return state{((m == v2) ? m : v1[0] + c * (v2 - v1[0]))};
            }
            else {
                ComputationalType v2_ = static_cast<ComputationalType>(v2);
                return state{((m == v2_) ? m : v1[0] + c * (v2_ - v1[0]))};
            }
        };
        util::transform(policy_, values_.cbegin(), values_.cend(), val, values_.begin(), func);
        return;
    }


    void updateWindow(const InputOutputType* val) {
        auto func
            = [](const state& v1, const InputOutputType& v2) { return state{static_cast<ComputationalType>(0.0)}; };
        util::transform(policy_, values_.begin(), values_.end(), val, values_.begin(), func);
        return;
    }


    ComputationalType icntpp() const { return static_cast<ComputationalType>(double(1.0) / double(win_.count())); };


    void print(std::ostream& os) const override { os << logHeader_; }
};

}  // namespace multio::action

#endif