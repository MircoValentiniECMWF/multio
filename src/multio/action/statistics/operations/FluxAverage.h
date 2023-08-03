
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/operations/OperationWithData.h"

namespace multio::action {

template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class FluxAverage final : public OperationWithData<T, 1> {
public:
    using state = std::array<T, 1>;
    using OperationWithData<T, 1>::name_;
    using OperationWithData<T, 1>::cfg_;
    using OperationWithData<T, 1>::logHeader_;
    using OperationWithData<T, 1>::values_;
    using OperationWithData<T, 1>::win_;
    using OperationWithData<T, 1>::checkSize;
    using OperationWithData<T, 1>::checkTimeInterval;

    FluxAverage(const std::string& name, long sz, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<T, 1>{name, "average", sz, true, win, cfg} {}

    FluxAverage(const std::string& name, long sz, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
                const StatisticsConfiguration& cfg) :
        OperationWithData<T, 1>{name, "average", sz, true, win, IOmanager, cfg} {};

    void compute(eckit::Buffer& buf) const override {
        checkTimeInterval();
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        auto val = static_cast<T*>(buf.data());
        cfg_.haveMissingValue() ? computeWithMissing(val) : computeWithoutMissing(val);
        return;
    }

    void updateData(const void* data, long sz) override {
        checkSize(sz);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const T* val = static_cast<const T*>(data);
        update(val);
        return;
    }

private:
    void computeWithMissing(T* buf) const {
        const double m = cfg_.missingValue();
        const double c
            = static_cast<double>(1.0) / static_cast<double>(win_.count() * cfg_.stepFreq() * cfg_.timeStep());
        std::transform(values_.cbegin(), values_.cend(), buf,
                       [c, m](const state& v) { return static_cast<T>(m == v[0] ? m : v[0] * c); });
        return;
    }

    void computeWithoutMissing(T* buf) const {
        const double c
            = static_cast<double>(1.0) / static_cast<double>(win_.count() * cfg_.stepFreq() * cfg_.timeStep());
        std::transform(values_.cbegin(), values_.cend(), buf, [c](const state& v) { return static_cast<T>(v[0] * c); });
        return;
    }

    void update(const T* val) {
        std::transform(values_.cbegin(), values_.cend(), val, values_.begin(),
                       [](const state& v1, const T& v2) { return state{v2}; });
        return;
    }

    void print(std::ostream& os) const override { os << logHeader_; }
};

}  // namespace multio::action
