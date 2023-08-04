
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/OperationActivation.h"
#include "multio/action/statistics/operations/OperationWithData.h"


#ifdef __ENABLE_AVERAGE_OPERATION__

namespace multio::action {

template <typename T, typename Q, typename = std::enable_if_t<std::is_floating_point<Q>::value>>
class Average final : public OperationWithData<T, 1> {
public:
    using state = std::array<T, 1>;
    using OperationWithData<T, 1>::init;
    using OperationWithData<T, 1>::name_;
    using OperationWithData<T, 1>::cfg_;
    using OperationWithData<T, 1>::logHeader_;
    using OperationWithData<T, 1>::values_;
    using OperationWithData<T, 1>::win_;
    using OperationWithData<T, 1>::checkSize;
    using OperationWithData<T, 1>::checkTimeInterval;

    Average(const std::string& name, long sz, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<T, 1>{name, "average", sz, true, win, cfg} {}

    Average(const std::string& name, long sz, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
            const StatisticsConfiguration& cfg) :
        OperationWithData<T, 1>{name, "average", sz, true, win, IOmanager, cfg} {};

    void compute(eckit::Buffer& buf) const override {
        checkTimeInterval();
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        Q* val = static_cast<Q*>(buf.data());
        compute(val);
        return;
    }

    void updateData(const void* data, long sz) override {
        checkSize(sz);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const Q* val = static_cast<const Q*>(data);
        cfg_.haveMissingValue() ? updateDataWithMissing(val) : updateDataWithoutMissing(val);
        return;
    }

    void updateWindow(const void* data, long sz) override {
        checkSize(sz);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const Q* val = static_cast<const Q*>(data);
        updateWindow(val);
        return;
    };

    void init(const void* data, long sz) override {
        checkSize(sz);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const Q* val = static_cast<const Q*>(data);
        // TODO: Used to save the first field of the window
        return;
    };

private:
    void compute(Q* buf) const {
        std::transform(values_.cbegin(), values_.cend(), buf, [](const state& v) { return static_cast<Q>(v[0]); });
        return;
    }

    void updateDataWithoutMissing(const Q* val) {
        const double c = icntpp();
        std::transform(values_.begin(), values_.end(), val, values_.begin(), [c](const state& v1, const Q& v2) {
            state out;
            Q v2_ = static_cast<Q>(v2);
            out[0] = v1[0] + c * (v2_ - v1[0]);
            return out;
        });
        return;
    }

    void updateDataWithMissing(const Q* val) {
        const double c = icntpp(), m = cfg_.missingValue();
        std::transform(values_.cbegin(), values_.cend(), val, values_.begin(), [c, m](const state& v1, const Q& v2) {
            state out;
            Q v2_ = static_cast<Q>(v2);
            out[0] = m == v2_ ? m : v1[0] + c * (v2_ - v1[0]);
            return out;
        });
        return;
    }

    void updateWindow(const Q* val) {
        std::transform(values_.begin(), values_.end(), val, values_.begin(), [](const state& v1, const Q& v2) {
            state out;
            out[0] = static_cast<Q>(0.0);
            return out;
        });
        return;
    }

    double icntpp() const { return double(1.0) / double(win_.count()); };
    void print(std::ostream& os) const override { os << logHeader_; }
};

}  // namespace multio::action

#endif