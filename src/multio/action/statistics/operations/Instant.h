
namespace multio::action {

template <typename T>
class Instant : public OperationWithData<T, 1> {
public:
    using OperationWithData<T, 1>::name_;
    using OperationWithData<T, 1>::cfg_;
    using OperationWithData<T, 1>::logHeader_;
    using OperationWithData<T, 1>::values_;
    using OperationWithData<T, 1>::win_;
    using OperationWithData<T, 1>::byte_size;
    using OperationWithData<T, 1>::checkSize;
    using OperationWithData<T, 1>::checkTimeInterval;


    Instant(long sz, const MovingWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<T, 1>{"instant", "instant", sz, true, win, cfg} {}

    Instant(long sz, const MovingWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
            const StatisticsConfiguration& cfg) :
        OperationWithData<T, 1>{"instant", "instant", sz, true, win, IOmanager, cfg} {};

    void compute(eckit::Buffer& buf) {
        checkTimeInterval();
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        buf.resize(byte_size());
        buf.zero();
        auto val = static_cast<T*>(buf.data());
        compute(val);
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
    void compute(T* val) {
        std::transform(values_.cbegin(), values_.cend(), val,
                       [](const std::array<T, 1>& v1) { return static_cast<T>(v1[0]); });
        return;
    }
    void update(const T* buf) {
        std::transform(values_.cbegin(), values_.cend(), buf, values_.begin(),
                       [](const std::array<T, 1>& v1, const T& v2) { return std::array<T, 1>{static_cast<T>(v2)}; });
        return;
    }
};
}  // namespace multio::action
