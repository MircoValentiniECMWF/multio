
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/OperationActivation.h"
#include "multio/action/statistics/operations/OperationWithData.h"

#ifdef __ENABLE_DIFFERENCE_OPERATION__
namespace multio::action {

template <typename T, typename Q, typename = std::enable_if_t<std::is_floating_point<Q>::value>>
class Difference final : public OperationWithData<T, 2> {
public:
    using state = std::array<T, 2>;
    using OperationWithData<T, 2>::init;
    using OperationWithData<T, 2>::name_;
    using OperationWithData<T, 2>::cfg_;
    using OperationWithData<T, 2>::logHeader_;
    using OperationWithData<T, 2>::values_;
    using OperationWithData<T, 2>::win_;
    using OperationWithData<T, 2>::checkSize;
    using OperationWithData<T, 2>::checkTimeInterval;

    Difference(const std::string& name, long sz, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<T, 2>{name, "difference", sz, true, win, cfg} {}

    Difference(const std::string& name, long sz, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
               const StatisticsConfiguration& cfg) :
        OperationWithData<T, 2>{name, "difference", sz, true, win, IOmanager, cfg} {};

    void compute(eckit::Buffer& buf) const override {
        checkTimeInterval();
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        Q* val = static_cast<Q*>(buf.data());
        cfg_.haveMissingValue() ? computeWithMissing(val) : computeWithoutMissing(val);
        buf.copy(values_.data(), values_.size() * sizeof(T));
        return;
    }

    void updateData(const void* data, long sz) override {
        checkSize(sz);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const Q* val = static_cast<const Q*>(data);
        update(val);
        return;
    }

    void updateWindow(const void* data, long sz) override {
        checkSize(sz);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const Q* val = static_cast<const Q*>(data);
        std::transform(values_.cbegin(), values_.cend(), val, values_.begin(), [](const state& v1, const Q& v2) {
            return state{static_cast<T>(v2), static_cast<T>(0.0)};
        });
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
    void computeWithMissing(Q* buf) const {
        const double m = cfg_.missingValue();
        std::transform(values_.cbegin(), values_.cend(), buf,
                       [m](const state& v) { return static_cast<Q>(m == v[0] ? m : v[1] - v[0]); });
        return;
    }

    void computeWithoutMissing(Q* buf) const {
        std::transform(values_.cbegin(), values_.cend(), buf,
                       [](const state& v) { return static_cast<Q>(v[1] - v[0]); });
        return;
    }

    void update(const Q* val) {
        std::transform(values_.begin(), values_.end(), val, values_.begin(), [](const state& v1, const Q& v2) {
            return state{v1[0], static_cast<T>(v2)};
        });
        return;
    }

    void print(std::ostream& os) const override { os << logHeader_; }
};

}  // namespace multio::action

#endif