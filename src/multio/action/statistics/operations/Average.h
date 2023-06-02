template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class Average final : public OperationWithData<T> {
public:
    using OperationWithData<T>::name_;
    using OperationWithData<T>::options_;
    using OperationWithData<T>::logHeader_;
    using OperationWithData<T>::values_;
    using OperationWithData<T>::count_;
    using OperationWithData<T>::checkSize;
    using OperationWithData<T>::checkTimeInterval;

    Average(const std::string& name, long sz, StatisticsOptions& options) :
        OperationWithData<T>{name, "average", sz, options}{}

    Average(const std::string& name, long sz, const std::string& partialPath, StatisticsOptions& options) :
        OperationWithData<T>{name, "average", sz, partialPath, options}{ };

    void compute( eckit::Buffer& buf ) override {
        checkTimeInterval();
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".compute().count=" << count_ << std::endl;
        buf.copy(values_.data(), values_.size() * sizeof(T) );
        return;
    }

    void update(const void* data, long sz, eckit::DateTime dt ) override {
        checkSize(sz);
        LOG_DEBUG_LIB(LibMultio) << logHeader_ << ".update().count=" << count_ << std::endl;
        const T* val = static_cast<const T*>(data);
        options_.haveMissingValue() ? updateWithMissing(val) : updateWithoutMissing(val);
        ++count_;
        return;
    }

private:
    void updateWithoutMissing( const T* val ){
        double c2 = icntpp(), c1 = sc(c2);        
        std::transform( values_.begin(), values_.end(), val, values_.begin(), [c1,c2](T v1, T v2){ return static_cast<T>(v1*c1 + v2*c2); } );
        return;
    }
    void updateWithMissing( const T* val ){
        const double c2 = icntpp(), c1 = sc(c2), m=options_.missingValue();   
        std::transform( values_.begin(), values_.end(), val, values_.begin(), [c1,c2,m](T v1, T v2){ return static_cast<T>(m==v2 ? m : v1*c1 + v2*c2); } );
        return;        
    }
    double icntpp() const { return double(1.0) / double(count_ + 1); };
    double sc( double v ) const { return double(count_) * v; };
    void print(std::ostream& os) const override { os << logHeader_; }
};