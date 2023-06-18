
namespace multio::action {

template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class Accumulate : public OperationWithData<T> {
public:
    using OperationWithData<T>::name_;
    using OperationWithData<T>::cfg_;
    using OperationWithData<T>::logHeader_;
    using OperationWithData<T>::values_;
    using OperationWithData<T>::win_;
    using OperationWithData<T>::byte_size;
    using OperationWithData<T>::checkSize;
    using OperationWithData<T>::checkTimeInterval;


    Accumulate(long sz, const MovingWindow& win, const StatisticsConfiguration& cfg) :
        OperationWithData<T>{"accumulate", "accumulate", sz, true, win, cfg} {}

    Accumulate(long sz, const MovingWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
               const StatisticsConfiguration& cfg) :
        OperationWithData<T>{"accumulate", "accumulate", sz, true, win, IOmanager, cfg} {};

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
        cfg_.haveMissingValue() ? updateWithMissing(val) : updateWithoutMissing(val);
        return;
    }

private:
    void updateWithoutMissing(const T* val) {
        std::transform(values_.begin(), values_.end(), val, values_.begin(),
                       [](T v1, T v2) { return static_cast<T>(v1 + v2); });
        return;
    }

    void updateWithMissing(const T* val) {
        double m = cfg_.missingValue();
        std::transform(values_.begin(), values_.end(), val, values_.begin(),
                       [m](T v1, T v2) { return static_cast<T>(m == v2 ? m : v1 + v2); });
        return;
    }
};

}  // namespace multio::action
