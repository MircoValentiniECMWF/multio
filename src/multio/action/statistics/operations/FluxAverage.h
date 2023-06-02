template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class FluxAverage final : public OperationWithData<T> {
public:
    using OperationWithData<T>::name_;
    using OperationWithData<T>::options_;
    using OperationWithData<T>::logHeader_;
    using OperationWithData<T>::values_;
    using OperationWithData<T>::count_;
    using OperationWithData<T>::checkSize;
    using OperationWithData<T>::checkTimeInterval;

    FluxAverage(const std::string& name, long sz, StatisticsOptions& options) :
        OperationWithData<T>{name, "average", sz, options}{}

    FluxAverage(const std::string& name, long sz, const std::string& partialPath, StatisticsOptions& options) :
        OperationWithData<T>{name, "average", sz, partialPath, options}{ };

    void compute( eckit::Buffer& buf ) override {
        checkTimeInterval( );
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << count_ << std::endl;
        auto val = static_cast<T*>(buf.data());
        options_.haveMissingValue() ? computeWithMissing(val) : computeWithoutMissing(val);
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
    void computeWithMissing( T* buf ){
        const double m=options_.missingValue();
        const double c=static_cast<double>(1.0)/static_cast<double>(count_ * options_.stepFreq() * options_.timeStep());
        std::transform( values_.begin(), values_.end(), buf, [c,m]( T v ){ return static_cast<T>(m==v ? m : v*c);} );
        return;
    }
    void computeWithoutMissing( T* buf ){
        const double c=static_cast<double>(1.0)/static_cast<double>(count_ * options_.stepFreq() * options_.timeStep());
        std::transform( values_.begin(), values_.end(), buf, [c]( T v ){ return static_cast<T>(v*c);} );
        return;
    }
    void print(std::ostream& os) const override { os << logHeader_; }

};


