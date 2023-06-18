
namespace multio::action {

template <typename T, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class OperationWithData : public OperationBase {
public:
    using OperationBase::cfg_;
    using OperationBase::logHeader_;
    using OperationBase::name_;

    OperationWithData(const std::string& name, const std::string& operation, long sz, bool needRestart,
                      const MovingWindow& win, const StatisticsConfiguration& cfg) :
        OperationBase{name, operation, win, cfg},
        values_{std::vector<T>(sz /= sizeof(T), 0.0)},
        needRestart_{needRestart} {}

    OperationWithData(const std::string& name, const std::string& operation, long sz, bool needRestart,
                      const MovingWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
                      const StatisticsConfiguration& cfg) :
        OperationBase{name, operation, win, cfg},
        values_{std::vector<T>(sz /= sizeof(T), 0.0)},
        needRestart_{needRestart} {
        load(IOmanager, cfg);
        return;
    }

    void updateWindow(const void* data, long sz) {
        std::transform(values_.begin(), values_.end(), values_.begin(), [](T v) { return static_cast<T>(0.0); });
        return;
    };

    void init(const void* data, long sz) {
        // TODO: Used to save the first field of the window
        return;
    };

    size_t byte_size() const { return values_.size() * sizeof(T); };

    void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) const {
        if (needRestart_) {
            std::vector<std::uint64_t>& restartState = IOmanager->getBuffer(restartSize());
            serialize(restartState);
            IOmanager->write(name_, restartSize());
            IOmanager->flush();
        }
        return;
    };

    void load(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) {
        if (needRestart_) {
            std::vector<std::uint64_t>& restartState = IOmanager->getBuffer(restartSize());
            IOmanager->read(name_, restartSize());
            deserialize(restartState);
        }
        return;
    };

protected:
    void serialize(std::vector<std::uint64_t>& restartState) const {
        std::transform(values_.cbegin(), values_.cend(), restartState.begin(), [](const T& v) {
            T lv = v;
            double dv = static_cast<double>(lv);
            return *reinterpret_cast<uint64_t*>(&dv);
        });
        restartState[restartSize() - 1] = computeChecksum(restartState, restartSize());
        return;
    };

    void deserialize(const std::vector<std::uint64_t>& restartState) {
        if (restartState[restartSize() - 1] != computeChecksum(restartState, restartSize())) {
            std::cout << restartState << std::endl;
            std::cout << restartState[restartSize() - 1] << std::endl;
            std::cout << computeChecksum(restartState, restartSize()) << std::endl;
            throw eckit::SeriousBug("Checksum mismatch!", Here());
        }
        auto last = restartState.cbegin() + restartSize();
        std::transform(restartState.cbegin(), --last, values_.begin(), [](const std::uint64_t& v) {
            std::uint64_t lv = v;
            double dv = *reinterpret_cast<double*>(&lv);
            return static_cast<T>(dv);
        });
        return;
    };

    void checkSize(long sz) {
        if (values_.size() != static_cast<long>(sz / sizeof(T))) {
            throw eckit::AssertionFailed(logHeader_ + " :: Expected size: " + std::to_string(values_.size())
                                         + " -- actual size: " + std::to_string(sz));
        }
    };

    void checkTimeInterval() {
        long sec = win_.count() * cfg_.stepFreq() * cfg_.timeStep();
        if (sec == 0) {
            throw eckit::SeriousBug{logHeader_ + " :: Divide by zero", Here()};
        }
        return;
    };

    size_t restartSize() const { return values_.size() + 1; }
    std::vector<T> values_;

private:
    bool needRestart_;
};

}  // namespace multio::action
