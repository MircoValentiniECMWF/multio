
namespace multio::action {

template <typename T, size_t N>
class OperationWithData : public OperationBase {
public:
    using OperationBase::cfg_;
    using OperationBase::logHeader_;
    using OperationBase::name_;

    OperationWithData(const std::string& name, const std::string& operation, long sz, bool needRestart,
                      const MovingWindow& win, const StatisticsConfiguration& cfg) :
        OperationBase{name, operation, win, cfg},
        needRestart_{needRestart},
        values_{std::vector<std::array<T, N>>{sz /= sizeof(T), std::array<T, N>{0.0}}} {}

    OperationWithData(const std::string& name, const std::string& operation, long sz, bool needRestart,
                      const MovingWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
                      const StatisticsConfiguration& cfg) :
        OperationBase{name, operation, win, cfg},
        needRestart_{needRestart},
        values_{std::vector<std::array<T, N>>{sz /= sizeof(T), std::array<T, N>{0.0}}} {
        load(IOmanager, cfg);
        return;
    }

    void updateWindow(const void* data, long sz) {
        std::transform(values_.cbegin(), values_.cend(), values_.begin(),
                       [](const std::array<T, N>& v) { return std::array<T, N>{static_cast<T>(0.0)}; });
        return;
    };

    size_t byte_size() const { return values_.size() * sizeof(T); };

    void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) const {
        if (needRestart_) {
            IOBuffer restartState{IOmanager->getBuffer(restartSize())};
            restartState.zero();
            serialize(restartState);
            IOmanager->write(name_, restartSize());
            IOmanager->flush();
        }
        return;
    };

    void load(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) {
        if (needRestart_) {
            IOBuffer restartState{IOmanager->getBuffer(restartSize())};
            IOmanager->read(name_, restartSize());
            deserialize(restartState);
            restartState.zero();
        }
        return;
    };

protected:
    void serialize(IOBuffer& restartState) const {
        size_t cnt = 0;
        for (size_t i = 0; i < values_.size(); ++i) {
            for (int j = 0; j < N; j++) {
                double dv = static_cast<double>(values_[i][j]);
                restartState[cnt] = *reinterpret_cast<uint64_t*>(&dv);
                cnt++;
            }
        }
        restartState.computeChecksum();
        return;
    };

    void deserialize(const IOBuffer& restartState) {
        restartState.checkChecksum();
        for (size_t i = 0; i < restartState.size() - 1; ++i) {
            std::uint64_t lv = restartState[i];
            double dv = *reinterpret_cast<double*>(&lv);
            size_t ii = i / N;
            size_t jj = i % N;
            values_[ii][jj] = static_cast<T>(dv);
        }
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


    bool needRestart_;
    std::vector<std::array<T, N>> values_;

private:
    size_t restartSize() const { return N * values_.size() + 1; };
};

}  // namespace multio::action
