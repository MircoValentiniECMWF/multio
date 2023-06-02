//==== Derived classes ============================
template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class Instant final : public  OperationWithData<T> {
public:
    using OperationWithData<T>::name_;
    using OperationWithData<T>::options_;
    using OperationWithData<T>::logHeader_;
    using OperationWithData<T>::values_;
    using OperationWithData<T>::count_;
    using OperationWithData<T>::checkSize;
    using OperationWithData<T>::checkTimeInterval;


    Instant(const std::string& name, long sz, StatisticsOptions& options) :
        OperationWithData<T>{name, "instant", sz, options}{}

    Instant(const std::string& name, long sz, const std::string& partialPath, StatisticsOptions& options) :
        OperationWithData<T>{name, "instant", sz, partialPath, options}{ };

    void compute( eckit::Buffer& buf ) override { 
        checkTimeInterval();
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << count_ << std::endl;
        buf.copy( values_.data(), values_.size() * sizeof(T));
        return;
    }

    void update(const void* data, long sz, eckit::DateTime dt ) override {
        checkSize(sz);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << count_ << std::endl;
        const T* val = static_cast<const T*>(data);
        std::copy(val, val + sz, values_.begin());
        ++count_;
        return;
    }

private:
    void print(std::ostream& os) const override { os << logHeader_; }
};