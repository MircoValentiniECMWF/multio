template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class Minimum final : public OperationWithData<T> {
public:
    using OperationWithData<T>::name_;
    using OperationWithData<T>::options_;
    using OperationWithData<T>::logHeader_;
    using OperationWithData<T>::values_;
    using OperationWithData<T>::count_;
    using OperationWithData<T>::checkSize;
    using OperationWithData<T>::checkTimeInterval;


    Minimum(const std::string& name, long sz, StatisticsOptions& options) :
        OperationWithData<T>{name, "minimum", sz, options}{}

    Minimum(const std::string& name, long sz, const std::string& partialPath, StatisticsOptions& options) :
        OperationWithData<T>{name, "minimum", sz, partialPath, options}{ };

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
        options_.haveMissingValue() ? updateWithMissing(val) : updateWithoutMissing(val);
        ++count_;
        return;
    }


private:
    void updateWithoutMissing( const T* val ){       
        std::transform( values_.begin(), values_.end(), val, values_.begin(), [](T v1, T v2){ return static_cast<T>(v1<v2 ? v1 : v2); } );
        return;
    }
    void updateWithMissing( const T* val ){
        double m=options_.missingValue();   
        std::transform( values_.begin(), values_.end(), val, values_.begin(), [m](T v1, T v2){ return static_cast<T>(m==v2 ? m : v1<v2 ? v1 : v2); } );
        return;        
    }
    void print(std::ostream& os) const override { os << logHeader_; }
};