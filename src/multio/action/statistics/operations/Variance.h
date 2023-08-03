
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/operations/OperationWithData.h"

namespace multio::action {

template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class Variance : public OperationWithData<T, 2> {
public:
    using state = std::array<T, 2>;
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

    void compute(eckit::Buffer& buf) const override {
        checkTimeInterval();
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        auto val = static_cast<T*>(buf.data());
        cfg_.haveMissingValue() ? computeWithMissing(val) : computeWithoutMissing(val);
        buf.copy(values_.data(), values_.size() * sizeof(T));
        return;
    }

    void updateData(const void* data, long sz) override {
        checkSize(sz);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const T* val = static_cast<const T*>(data);
        cfg_.haveMissingValue() ? updateWithMissing(val) : updateWithoutMissing(val);
        return;
    }

private:
    void updateWithoutMissing(const T* val) {
        const double c = icntpp();
        std::transform(values_.begin(), values_.end(), val, values_.begin(), [c](const state& v1, const T& v2) {
            state out;
            out[0] = v1[0] + c * (v2 - v1[0]);
            out[1] = v1[1] + (v2 - v1[0]) * (v2 - out[0]);
            return out;
        });
        return;
    }
    void updateWithMissing(const T* val) {
        const double c = icntpp(), m = cfg_.missingValue();
        std::transform(values_.cbegin(), values_.cend(), val, values_.begin(), [c, m](const state& v1, const T& v2) {
            state out;
            out[0] = m == v2 ? m : v1[0] + c * (v2 - v1[0]);
            out[1] = m == v2 ? m : v1[1] + (v2 - v1[0]) * (v2 - out[0]);
            return out;
        });
        return;
    }

    void computeWithMissing(T* buf) const {
        const double c = icntpp(), m = cfg_.missingValue();
        std::transform(values_.cbegin(), values_.cend(), buf,
                       [c, m](const state& v) { return static_cast<T>(m == v[1] ? m : v[1] * c); });
        return;
    }

    void computeWithoutMissing(T* buf) const {
        const double c = icntpp();
        std::transform(values_.cbegin(), values_.cend(), buf, [c](const state& v) { return static_cast<T>(v[1] * c); });
        return;
    }

    void print(std::ostream& os) const override { os << logHeader_; }

    double icntpp() const { return double(1.0) / double(win_.count()); };
};

}  // namespace multio::action