#pragma once

#include "eckit/types/DateTime.h"
#include "multio/action/statistics/StatisticsOptions.h"

namespace multio {
namespace action {
class DateTimePeriod {
public:
    DateTimePeriod(const std::string& partialPath, const StatisticsOptions& options);
    DateTimePeriod(const eckit::DateTime& startPoint, eckit::Second duration, long offset);
    DateTimePeriod(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint, long offset);
    DateTimePeriod(const eckit::DateTime& startPoint, eckit::Second duration);
    DateTimePeriod(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint);


    void reset(const eckit::DateTime& current);
    void reset(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint);

    bool isWithin(const eckit::DateTime& dt);

    long timeSpanInSeconds() const;
    eckit::DateTime endPoint() const;
    eckit::DateTime startPoint() const;
    void dump(const std::string& name, long step) const;

private:
    eckit::DateTime startPoint_;
    eckit::DateTime endPoint_;
    long offset_;

    void print(std::ostream& os) const;

    friend std::ostream& operator<<(std::ostream& os, const DateTimePeriod& a);
};

}  // namespace action
}  // namespace multio
