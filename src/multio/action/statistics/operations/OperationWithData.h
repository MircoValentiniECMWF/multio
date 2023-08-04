
#pragma once

#include <array>

#include "multio/action/statistics/operations/Operation.h"

#include "eckit/exception/Exceptions.h"
namespace multio::action {

template <typename T, size_t nStates, typename = std::enable_if_t<std::is_floating_point<T>::value>>
class OperationWithData : public Operation {
public:
    using Operation::cfg_;
    using Operation::logHeader_;
    using Operation::name_;
    using Operation::profiler_;

    OperationWithData(const std::string& name, const std::string& operation, long sz, bool needRestart,
                      const OperationWindow& win, const StatisticsConfiguration& cfg) :
        Operation{name, operation, win, cfg},
        values_{std::vector<std::array<T, nStates>>(sz /= sizeof(T), std::array<T, nStates>{0.0})},
        needRestart_{needRestart} {}

    OperationWithData(const std::string& name, const std::string& operation, long sz, bool needRestart,
                      const OperationWindow& win, std::shared_ptr<StatisticsIO>& IOmanager,
                      const StatisticsConfiguration& cfg) :
        Operation{name, operation, win, cfg},
        values_{std::vector<std::array<T, nStates>>(sz /= sizeof(T), std::array<T, nStates>{0.0})},
        needRestart_{needRestart} {
        load(IOmanager, cfg);
        return;
    }

    void init() override {
        // TODO: Used to save the initialization time of the window
        return;
    };


    size_t numberOfStates() const override final { return nStates; };
    size_t byte_size() const override final { return values_.size() * sizeof(T); };
    size_t memory_in_bytes() const final { return values_.size() * sizeof(T) * nStates; };

    void dump(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) const override {
        profiler_[4].tic();
        if (needRestart_) {
            IOBuffer restartState{IOmanager->getBuffer(restartSize())};
            restartState.zero();
            serialize(restartState);
            IOmanager->write(name_, restartSize());
            IOmanager->flush();
        }
        profiler_[4].toc();
        return;
    };

    void load(std::shared_ptr<StatisticsIO>& IOmanager, const StatisticsConfiguration& cfg) override {
        profiler_[5].tic();
        if (needRestart_) {
            IOBuffer restartState{IOmanager->getBuffer(restartSize())};
            IOmanager->read(name_, restartSize());
            deserialize(restartState);
            restartState.zero();
        }
        profiler_[5].toc();
        return;
    };

protected:
    void serialize(IOBuffer& restartState) const {
        size_t cnt = 0;
        for (size_t i = 0; i < values_.size(); ++i) {
            for (int j = 0; j < nStates; j++) {
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
            size_t ii = i / nStates;
            size_t jj = i % nStates;
            values_[ii][jj] = static_cast<T>(dv);
        }
        return;
    };

    void checkSize(long sz) const {
        if (values_.size() != static_cast<long>(sz / sizeof(T))) {
            std::ostringstream os;
            os << logHeader_ + " :: Expected size: " + std::to_string(values_.size())
                      + " -- actual size: " + std::to_string(sz)
               << std::endl;
            throw eckit::AssertionFailed(os.str());
        }
    };

    void checkTimeInterval() const {
        long sec = win_.count() * cfg_.stepFreq() * cfg_.timeStep();
        if (sec == 0) {
            throw eckit::SeriousBug{logHeader_ + " :: Divide by zero", Here()};
        }
        return;
    };

    size_t restartSize() const { return values_.size() + 1; }
    std::vector<std::array<T, nStates>> values_;

private:
    bool needRestart_;
};

}  // namespace multio::action
