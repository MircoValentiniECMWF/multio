#pragma once

#include "eckit/types/DateTime.h"
#include "multio/action/statistics/StatisticsOptions.h"

namespace multio {
namespace action {


eckit::DateTime epochDateTime(const message::Message& msg, const StatisticsOptions& options);
eckit::DateTime prevDateTime(const message::Message& msg, const StatisticsOptions& options);
eckit::DateTime currentDateTime(const message::Message& msg, const StatisticsOptions& options);
eckit::DateTime nextDateTime(const message::Message& msg, const StatisticsOptions& options);
eckit::DateTime winStartDateTime(const message::Message& msg, const StatisticsOptions& options);

class DateTimePeriod {
public:
    DateTimePeriod(const std::string& partialPath, const char* periodKind, const StatisticsOptions& options);
    DateTimePeriod(const eckit::DateTime& startPoint, const eckit::DateTime& creationPoint,
                   const eckit::DateTime& endPoint, const char* periodKind);

    void reset(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint);

    bool isWithin(const eckit::DateTime& dt);

    long timeSpanInSeconds() const;
    eckit::DateTime endPoint() const;
    eckit::DateTime startPoint() const;
    eckit::DateTime creationPoint() const;
    void dump(const std::string& name, long step) const;

private:
    eckit::DateTime startPoint_;
    eckit::DateTime endPoint_;
    eckit::DateTime creationPoint_;
    const char* periodKind_;
    void print(std::ostream& os) const;

    friend std::ostream& operator<<(std::ostream& os, const DateTimePeriod& a);
};

}  // namespace action
}  // namespace multio
