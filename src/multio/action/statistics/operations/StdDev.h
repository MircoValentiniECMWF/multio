namespace multio::action {
template <typename T>
class StdDev final : public OperationWithData<T, 2> {
public:
    using OperationWithData<T, 2>::name_;
    using OperationWithData<T, 2>::cfg_;
    using OperationWithData<T, 2>::logHeader_;
    using OperationWithData<T, 2>::values_;
    using OperationWithData<T, 2>::win_;
    using OperationWithData<T, 2>::checkSize;
    using OperationWithData<T, 2>::checkTimeInterval;

    StdDev(long sz, const MovingWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<T, 2>{"stddev", "stddev", sz, true, win, cfg} {}

    StdDev(long sz, const MovingWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
           const StatisticsConfiguration& cfg) :
        OperationWithData<T, 2>{"stddev", "stddev", sz, true, win, IOmanager, cfg} {};

    void init(const void* data, long sz) { return; };

    bool needInit() const { return false; };

    void compute(eckit::Buffer& buf) {
        checkTimeInterval();
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        auto val = static_cast<T*>(buf.data());
        cfg_.haveMissingValue() ? computeWithMissing(val) : computeWithoutMissing(val);
        return;
    }

    void updateData(const void* data, long sz) {
        checkSize(sz);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const T* val = static_cast<const T*>(data);
        cfg_.haveMissingValue() ? updateWithMissing(val) : updateWithoutMissing(val);
        return;
    }

private:
    void updateWithMissing(const T* val) {
        const double c2 = icntpp(), c1 = sc(c2), m = cfg_.missingValue();
        std::transform(
            values_.cbegin(), values_.cend(), val, values_.begin(), [c1, c2, m](const std::array<T, 2>& v1, T v2) {
                return m == v2 ? std::array<T, 2>{static_cast<T>(m), static_cast<T>(m)}
                               : std::array<T, 2>{static_cast<T>(v1[0] * c1 + v2 * c2),
                                                  static_cast<T>(v1[1] * c1 + (v1[0] - v2) * (v1[0] - v2) * c1 * c2)};
            });
        return;
    }
    void updateWithoutMissing(const T* val) {
        const double c2 = icntpp(), c1 = sc(c2);
        std::transform(values_.cbegin(), values_.cend(), val, values_.begin(),
                       [c1, c2](const std::array<T, 2>& v1, T v2) {
                           return std::array<T, 2>{static_cast<T>(v1[0] * c1 + v2 * c2),
                                                   static_cast<T>(v1[1] * c1 + (v1[0] - v2) * (v1[0] - v2) * c1 * c2)};
                       });
        return;
    }
    void computeWithoutMissing(T* buf) {
        std::transform(values_.cbegin(), values_.cend(), buf,
                       [](const std::array<T, 2>& v1) { return static_cast<T>(std::sqrt(v1[1])); });
        return;
    }
    void computeWithMissing(T* buf) {
        const double m = cfg_.missingValue();
        std::transform(values_.cbegin(), values_.cend(), buf, [m](const std::array<T, 2>& v1) {
            return static_cast<T>(m == v1[1] ? m : static_cast<T>(std::sqrt(v1[1])));
        });
        return;
    }
    double icntpp() const { return double(1.0) / double(win_.count()); };
    double sc(double v) const { return double(win_.count() - 1) * v; };
};
}  // namespace multio::action