
#pragma once

#include "multio/LibMultio.h"
#include "multio/action/statistics/OperationActivation.h"
#include "multio/action/statistics/operations/OperationWithData.h"


#ifdef __ENABLE_INSTANT_OPERATION__
namespace multio::action {

template <typename T, typename Q, typename = std::enable_if_t<std::is_floating_point<Q>::value>>
class Instant final : public OperationWithData<T, 1> {
public:
    using state = std::array<T, 1>;
    using OperationWithData<T, 1>::init;
    using OperationWithData<T, 1>::needStepZero;
    using OperationWithData<T, 1>::profiler_;
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
        const Q* val = static_cast<const Q*>(data.data());
        update(val);
        profiler_[1].toc();
        return;
    };

    void updateWindow(const eckit::Buffer& data) override {
        profiler_[2].tic();
        checkSize(data.size());
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const Q* val = static_cast<const Q*>(data.data());
        updateWindow(val);
        profiler_[2].toc();
        return;
    };

    void compute(eckit::Buffer& data) const override {
        profiler_[3].tic();
        checkSize(data.size());
        checkTimeInterval();
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        Q* val = static_cast<Q*>(data.data());
        compute(val);
        profiler_[3].toc();
        return;
    };

private:
    void compute(Q* buf) const {
        std::transform(values_.cbegin(), values_.cend(), buf, [](const state& v) { return static_cast<Q>(v[0]); });
        return;
    }

    void update(const Q* val) {
        std::transform(values_.cbegin(), values_.cend(), val, values_.begin(),
                       [](const state& v1, const Q& v2) { return state{static_cast<T>(v2)}; });
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

    void print(std::ostream& os) const override { os << logHeader_; }
};
}  // namespace multio::action

#endif