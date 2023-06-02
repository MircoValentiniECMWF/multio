#pragma once

#include <map>
#include <memory>
#include <string>

#include "multio/action/statistics/Operation.h"
#include "multio/action/statistics/Period.h"
#include "multio/action/statistics/StatisticsOptions.h"
#include "multio/message/Message.h"
#include "eckit/types/DateTime.h"


namespace multio ::action {


class TemporalStatistics {
public:

    using op = std::vector<std::unique_ptr<Operation>>;

    op::iterator begin() {return statistics_.begin();};
    op::iterator end() {return statistics_.end();};
    op::const_iterator cbegin() const {return statistics_.cbegin();};
    op::const_iterator cend() const {return statistics_.cend();};

    static std::unique_ptr<TemporalStatistics> build(const std::string& unit, long span,
                                                     const std::vector<std::string>& operations,
                                                     const message::Message& msg, const std::string& partialPath,
                                                           StatisticsOptions& options);

    // Restart constructor
    TemporalStatistics(const std::vector<std::string>& operations, const DateTimePeriod& period,
                       const message::Message& msg, const std::string& partialPath, StatisticsOptions& options,
                       long span);

    virtual ~TemporalStatistics() = default;


    bool isEndOfWindow(message::Message& msg, StatisticsOptions& options);
    void update(message::Message& msg, StatisticsOptions& options);
    void reset(const message::Message& msg, StatisticsOptions& options);
    void dump(long step, StatisticsOptions& options) const;

    std::string stepRange(long step);
    const DateTimePeriod& current() const;
    long startStep() const;

    virtual void print(std::ostream& os) const = 0;

protected:
    long span_;
    std::string name_;
    std::string partialPath_;
    long prevStep_;
    DateTimePeriod current_;
    std::vector<std::string> opNames_;
    std::vector<std::unique_ptr<Operation>> statistics_;

private:

    virtual void resetPeriod(const message::Message& msg, StatisticsOptions& options) = 0;
    void checkName( const std::string& name );
    void checkIsWithin( message::Message& msg, StatisticsOptions& options );

    friend std::ostream& operator<<(std::ostream& os, const TemporalStatistics& a) {
        a.print(os);
        return os;
    }
};

}  // namespace action

