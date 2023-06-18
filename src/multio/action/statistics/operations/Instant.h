
namespace multio::action {

template <typename T>
class Instant : public OperationWithData<T> {
public:
    using OperationWithData<T>::name_;
    using OperationWithData<T>::cfg_;
    using OperationWithData<T>::logHeader_;
    using OperationWithData<T>::values_;
    using OperationWithData<T>::win_;
    using OperationWithData<T>::byte_size;
    using OperationWithData<T>::checkSize;
    using OperationWithData<T>::checkTimeInterval;


    Instant(long sz, const MovingWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<T>{"instant", "instant", sz, true, win, cfg} {}

    Instant(long sz, const MovingWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
            const StatisticsConfiguration& cfg) :
        OperationWithData<T>{"instant", "instant", sz, true, win, IOmanager, cfg} {};

    void compute(eckit::Buffer& buf) {
        checkTimeInterval();
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << win_.count() << std::endl;
        buf.resize(byte_size());
        buf.zero();
        buf.copy(values_.data(), values_.size() * sizeof(T));
        return;
    }

    void updateData(const void* data, long sz) {
        checkSize(sz);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << win_.count() << std::endl;
        const T* val = static_cast<const T*>(data);
        std::copy(val, val + sz, values_.begin());
        return;
    }
};
}  // namespace multio::action
