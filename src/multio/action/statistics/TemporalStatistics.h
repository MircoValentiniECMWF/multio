#pragma once

#include <map>
#include <memory>
#include <string>

#include "multio/action/statistics/Operation.h"
#include "multio/action/statistics/Period.h"
#include "multio/action/statistics/StatisticsOptions.h"
#include "multio/message/Message.h"


namespace multio {
namespace action {


class TemporalStatistics {
public:
    static std::unique_ptr<TemporalStatistics> build(const std::string& unit, long span,
                                                     const std::vector<std::string>& operations,
                                                     const message::Message& msg, const std::string& partialPath,
                                                     const StatisticsOptions& options);

    // Restart constructor
    TemporalStatistics(const std::vector<std::string>& operations, const DateTimePeriod& period,
                       const message::Message& msg, const std::string& partialPath, const StatisticsOptions& options,
                       long span);

    virtual ~TemporalStatistics() = default;
    bool process(message::Message& msg, const StatisticsOptions& options);
    std::map<std::string, eckit::Buffer> compute(const message::Message& msg);
    std::string stepRange(long step);
    const DateTimePeriod& current() const;
    void reset(const message::Message& msg);
    long startStep() const { return prevStep_; };
    virtual void print(std::ostream& os) const = 0;
    void dump(long step, const StatisticsOptions& options) const;

protected:
    long span_;
    std::string name_;
    std::string partialPath_;
    long prevStep_;
    DateTimePeriod current_;
    StatisticsOptions options_;
    std::vector<std::string> opNames_;
    std::vector<OperationVar> statistics_;

    void updateStatistics(const message::Message& msg);

private:
    virtual bool process_next(message::Message& msg);

    virtual void resetPeriod(const message::Message& msg) = 0;


    friend std::ostream& operator<<(std::ostream& os, const TemporalStatistics& a) {
        a.print(os);
        return os;
    }
};

}  // namespace action
}  // namespace multio
