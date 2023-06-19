namespace multio::action {
template <typename T>
class Difference final : public OperationWithData<T, 2> {
public:
    using OperationWithData<T, 2>::name_;
    using OperationWithData<T, 2>::cfg_;
    using OperationWithData<T, 2>::logHeader_;
    using OperationWithData<T, 2>::values_;
    using OperationWithData<T, 2>::win_;
    using OperationWithData<T, 2>::checkSize;
    using OperationWithData<T, 2>::checkTimeInterval;

    Difference(long sz, const MovingWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<T, 2>{"difference", "difference", sz, true, win, cfg} {}

    Difference(long sz, const MovingWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
               const StatisticsConfiguration& cfg) :
        OperationWithData<T, 2>{"difference", "difference", sz, true, win, IOmanager, cfg} {};


    void init(const void* data, long sz) {
        checkSize(sz);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".init().count=" << win_.count() << std::endl;
        const T* val = static_cast<const T*>(data);
        cfg_.haveMissingValue() ? initWithMissing(val) : initWithoutMissing(val);
        return;
    };

    bool needInit() const { return true; };

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
        cfg_.haveMissingValue() ? updateDataWithMissing(val) : updateDataWithoutMissing(val);
        return;
    }

    void updateWindow(const void* data, long sz) {
        checkSize(sz);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const T* val = static_cast<const T*>(data);
        cfg_.haveMissingValue() ? updateWindowWithMissing(val) : updateWindowWithoutMissing(val);
        return;
    }

private:
    void updateDataWithMissing(const T* val) {
        const double m = cfg_.missingValue();
        std::transform(values_.cbegin(), values_.cend(), val, values_.begin(), [m](const std::array<T, 2>& v1, T v2) {
            return m == v2 ? std::array<T, 2>{static_cast<T>(m), static_cast<T>(m)}
                           : std::array<T, 2>{static_cast<T>(v1[0]), static_cast<T>(v2)};
        });
        return;
    }
    void updateDataWithoutMissing(const T* val) {
        std::transform(values_.cbegin(), values_.cend(), val, values_.begin(), [](const std::array<T, 2>& v1, T v2) {
            return std::array<T, 2>{static_cast<T>(v1[0]), static_cast<T>(v2)};
        });
        return;
    }
    void updateWindowWithMissing(const T* val) {
        const double m = cfg_.missingValue();
        std::transform(values_.cbegin(), values_.cend(), val, values_.begin(), [m](const std::array<T, 2>& v1, T v2) {
            return m == v2 ? std::array<T, 2>{static_cast<T>(m), static_cast<T>(m)}
                           : std::array<T, 2>{static_cast<T>(v2), static_cast<T>(0.0)};
        });
        return;
    }
    void updateWindowWithoutMissing(const T* val) {
        std::transform(values_.cbegin(), values_.cend(), val, values_.begin(), [](const std::array<T, 2>& v1, T v2) {
            return std::array<T, 2>{static_cast<T>(v2), static_cast<T>(0.0)};
        });
        return;
    }
    void computeWithMissing(T* buf) {
        const double m = cfg_.missingValue();
        std::transform(values_.cbegin(), values_.cend(), buf, [m](const std::array<T, 2>& v1) {
            return static_cast<T>(m == v1[1] ? m : static_cast<T>(v1[1] - v1[0]));
        });
        return;
    }
    void computeWithoutMissing(T* buf) {
        std::transform(values_.cbegin(), values_.cend(), buf,
                       [](const std::array<T, 2>& v1) { return static_cast<T>(v1[1] - v1[0]); });
        return;
    }
    void initWithMissing(const T* val) {
        const double m = cfg_.missingValue();
        std::transform(values_.cbegin(), values_.cend(), val, values_.begin(), [m](const std::array<T, 2>& v1, T v2) {
            return m == v2 ? std::array<T, 2>{static_cast<T>(m), static_cast<T>(m)}
                           : std::array<T, 2>{static_cast<T>(v2), static_cast<T>(0.0)};
        });
        return;
    }
    void initWithoutMissing(const T* val) {
        std::transform(values_.cbegin(), values_.cend(), val, values_.begin(), [](const std::array<T, 2>& v1, T v2) {
            return std::array<T, 2>{static_cast<T>(v2), static_cast<T>(0.0)};
        });
        return;
    }
};
}  // namespace multio::action