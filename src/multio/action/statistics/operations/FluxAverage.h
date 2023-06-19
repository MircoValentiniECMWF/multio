
namespace multio::action {

template <typename T>
class FluxAverage : public OperationWithData<T, 1> {
public:
    using OperationWithData<T, 1>::name_;
    using OperationWithData<T, 1>::cfg_;
    using OperationWithData<T, 1>::logHeader_;
    using OperationWithData<T, 1>::values_;
    using OperationWithData<T, 1>::win_;
    using OperationWithData<T, 1>::byte_size;
    using OperationWithData<T, 1>::checkSize;
    using OperationWithData<T, 1>::checkTimeInterval;

    FluxAverage(long sz, const MovingWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<T, 1>{"flux-average", "average", sz, true, win, cfg} {}

    FluxAverage(long sz, const MovingWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
                const StatisticsConfiguration& cfg) :
        OperationWithData<T, 1>{"flux-average", "average", sz, true, win, IOmanager, cfg} {};

    void compute(eckit::Buffer& buf) {
        checkTimeInterval();
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        buf.resize(byte_size());
        buf.zero();
        auto val = static_cast<T*>(buf.data());
        cfg_.haveMissingValue() ? computeWithMissing(val) : computeWithoutMissing(val);
        return;
    }

    void updateData(const void* data, long sz) {
        checkSize(sz);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const T* val = static_cast<const T*>(data);
        update(val);
        return;
    }

private:
    void computeWithMissing(T* buf) {
        const double m = cfg_.missingValue();
        const double c
            = static_cast<double>(1.0) / static_cast<double>(win_.count() * cfg_.stepFreq() * cfg_.timeStep());
        std::transform(values_.cbegin(), values_.cend(), buf,
                       [c, m](const std::array<T, 1>& v) { return static_cast<T>(m == v[0] ? m : v[0] * c); });
        return;
    }

    void computeWithoutMissing(T* buf) {
        const double c
            = static_cast<double>(1.0) / static_cast<double>(win_.count() * cfg_.stepFreq() * cfg_.timeStep());
        std::transform(values_.cbegin(), values_.cend(), buf,
                       [c](const std::array<T, 1>& v) { return static_cast<T>(v[0] * c); });
        return;
    }

    void update(const T* buf) {
        std::transform(values_.cbegin(), values_.cend(), buf, values_.begin(),
                       [](const std::array<T, 1>& v1, const T& v2) { return std::array<T, 1>{v2}; });
        return;
    }
};

}  // namespace multio::action
