
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/OperationActivation.h"
#include "multio/action/statistics/operations/OperationWithData.h"

namespace multio::action {

#ifdef __ENABLE_VARIANCE_OPERATION__

template <typename T, typename Q, typename = std::enable_if_t<std::is_floating_point<Q>::value>>
class Variance : public OperationWithData<T, 2> {
public:
    using state = std::array<T, 2>;
    using OperationWithData<T, 2>::init;
    using OperationWithData<T, 2>::profiler_;
    using OperationWithData<T, 2>::needStepZero;
    using OperationWithData<T, 2>::name_;
    using OperationWithData<T, 2>::cfg_;
    using OperationWithData<T, 2>::logHeader_;
    using OperationWithData<T, 2>::values_;
    using OperationWithData<T, 2>::win_;
    using OperationWithData<T, 2>::checkSize;
    using OperationWithData<T, 2>::checkTimeInterval;

    Variance(const std::string& name, long sz, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<T, 2>{name, "variance", sz, true, win, cfg} {}

    Variance(const std::string& name, long sz, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
             const StatisticsConfiguration& cfg) :
        OperationWithData<T, 2>{name, "variance", sz, true, win, IOmanager, cfg} {};


    bool needStepZero() const override { return false; };

    void init(const eckit::Buffer& data) override {
        profiler_[0].tic();
        checkSize(data.size());
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const Q* val = static_cast<const Q*>(data.data());
        profiler_[0].toc();
        // TODO: Used to save the first field of the window
        return;
    };

    void updateData(const eckit::Buffer& data) override {
        profiler_[1].tic();
        checkSize(data.size());
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const T* val = static_cast<const T*>(data.data());
        cfg_.haveMissingValue() ? updateWithMissing(val) : updateWithoutMissing(val);
        profiler_[1].toc();
        return;
    }

    void updateWindow(const eckit::Buffer& data) override {
        profiler_[2].tic();
        checkSize(data.size());
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const Q* val = static_cast<const Q*>(data.data());
        std::transform(values_.cbegin(), values_.cend(), val, values_.begin(), [](const state& v1, const Q& v2) {
            return state{static_cast<T>(0.0), static_cast<T>(0.0)};
        });
        profiler_[2].toc();
        return;
    };

    void compute(eckit::Buffer& data) const override {
        profiler_[3].tic();
        checkSize(data.size());
        checkTimeInterval();
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        auto val = static_cast<T*>(data.data());
        cfg_.haveMissingValue() ? computeWithMissing(val) : computeWithoutMissing(val);
        profiler_[3].toc();
        return;
    }


private:
    void updateWithoutMissing(const Q* val) {
        const double c = icntpp();
        std::transform(values_.begin(), values_.end(), val, values_.begin(), [c](const state& v1, const Q& v2) {
            state out;
            out[0] = v1[0] + c * (v2 - v1[0]);
            out[1] = v1[1] + (v2 - v1[0]) * (v2 - out[0]);
            return out;
        });
        return;
    }
    void updateWithMissing(const Q* val) {
        const double c = icntpp(), m = cfg_.missingValue();
        std::transform(values_.cbegin(), values_.cend(), val, values_.begin(), [c, m](const state& v1, const Q& v2) {
            state out;
            out[0] = m == v2 ? m : v1[0] + c * (v2 - v1[0]);
            out[1] = m == v2 ? m : v1[1] + (v2 - v1[0]) * (v2 - out[0]);
            return out;
        });
        return;
    }

    void computeWithMissing(Q* buf) const {
        const double c = icntpp(), m = cfg_.missingValue();
        std::transform(values_.cbegin(), values_.cend(), buf,
                       [c, m](const state& v) { return static_cast<Q>(m == v[1] ? m : v[1] * c); });
        return;
    }

    void computeWithoutMissing(Q* buf) const {
        const double c = icntpp();
        std::transform(values_.cbegin(), values_.cend(), buf, [c](const state& v) { return static_cast<Q>(v[1] * c); });
        return;
    }

    void print(std::ostream& os) const override { os << logHeader_; }

    double icntpp() const { return double(1.0) / double(win_.count()); };
};

}  // namespace multio::action

#endif