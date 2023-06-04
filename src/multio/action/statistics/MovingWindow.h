#pragma once

#include "eckit/types/DateTime.h"

#include "StatisticsConfiguration.h"
#include "StatisticsIO.h"
#include "multio/message/Message.h"

namespace multio::action {

class MovingWindow {
public:
    MovingWindow(StatisticsIO& IOmanager, const char* periodKind, const StatisticsConfiguration& cfg);
    MovingWindow(const eckit::DateTime& epochPoint, const eckit::DateTime& startPoint,
                 const eckit::DateTime& creationPoint, const eckit::DateTime& endPoint, long timeStepInSeconds,
                 const char* periodKind, long span);

    long count() const;

    void updateData(const eckit::DateTime& currentPoint);
    void updateWindow(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint);

    void dump(StatisticsIO& IOmanager, const StatisticsConfiguration& cfg) const;
    void load(StatisticsIO& IOmanager, const StatisticsConfiguration& cfg);

    bool isWithin(const eckit::DateTime& dt) const;
    bool gtLowerBound(const eckit::DateTime& dt, bool throw_error) const;
    bool leUpperBound(const eckit::DateTime& dt, bool throw_error) const;

    long timeSpanInSeconds() const;
    long timeSpanInSteps() const;
    long lastPointsDiffInSeconds() const;

    long startStepInSeconds() const;
    long creationStepInSeconds() const;
    long currStepInSeconds() const;
    long prevStepInSeconds() const;
    long endStepInSeconds() const;

    long startStepInHours() const;
    long creationStepInHours() const;
    long currStepInHours() const;
    long prevStepInHours() const;
    long endStepInHours() const;

    long startPointInStep() const;
    long creationPointInStep() const;
    long currPointInStep() const;
    long prevPointInStep() const;
    long endStep() const;

    eckit::DateTime epochPoint() const;
    eckit::DateTime startPoint() const;
    eckit::DateTime creationPoint() const;
    eckit::DateTime currPoint() const;
    eckit::DateTime prevPoint() const;
    eckit::DateTime endPoint() const;

    const std::string stepRange() const;
    const std::string stepRangeInHours() const;

private:
    eckit::DateTime epochPoint_;
    eckit::DateTime startPoint_;
    eckit::DateTime endPoint_;
    eckit::DateTime creationPoint_;
    eckit::DateTime prevPoint_;
    eckit::DateTime currPoint_;
    long timeStepInSeconds_;
    long count_;

    void serialize(std::array<int64_t, 16>& currState) const;
    void deserialize(const std::array<int64_t, 16>& currState);

    void print(std::ostream& os) const;
    friend std::ostream& operator<<(std::ostream& os, const MovingWindow& a);
};

}  // namespace multio::action