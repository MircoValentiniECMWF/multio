
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/operations/OperationWithData.h"

namespace multio::action {

template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class Instant final : public OperationWithData<T, 1> {
public:
    using state = std::array<T, 1>;
    using OperationWithData<T, 1>::name_;
    using OperationWithData<T, 1>::cfg_;
    using OperationWithData<T, 1>::logHeader_;
    using OperationWithData<T, 1>::values_;
    using OperationWithData<T, 1>::win_;
    using OperationWithData<T, 1>::checkSize;
    using OperationWithData<T, 1>::checkTimeInterval;


    Instant(const std::string& name, long sz, const OperationWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<T, 1>{name, "instant", sz, true, win, cfg} {}

    Instant(const std::string& name, long sz, const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
            const StatisticsConfiguration& cfg) :
        OperationWithData<T, 1>{name, "instant", sz, true, win, IOmanager, cfg} {};

    void compute(eckit::Buffer& buf) const override {
        checkTimeInterval();
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        auto val = static_cast<T*>(buf.data());
        compute(val);
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
    void compute(T* buf) const {
        std::transform(values_.cbegin(), values_.cend(), buf, [](const state& v) { return static_cast<T>(v[0]); });
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
