
#include "Period.h"

#include <fstream>
#include <iostream>

#include "StatisticsIO.h"
#include "eckit/filesystem/PathName.h"
#include "multio/LibMultio.h"

#include "atlas_io/atlas-io.h"

namespace multio {
namespace action {

static eckit::LocalConfiguration no_compression = [] {
    eckit::LocalConfiguration c;
    c.set("compression", "none");
    return c;
}();

eckit::DateTime epochDateTime(const message::Message& msg, const StatisticsOptions& options) {
    eckit::Date startDate{options.startDate()};
    long startTime = options.startTime();
    auto hour = startTime / 10000;
    auto minute = (startTime % 10000) / 100;
    return eckit::DateTime{startDate, eckit::Time{hour, minute, 0}};
}


eckit::DateTime prevDateTime(const message::Message& msg, const StatisticsOptions& options) {
    return epochDateTime(msg, options)
         + static_cast<eckit::Second>(std::max((options.step() - 1L), 0L) * options.timeStep());
}


eckit::DateTime currentDateTime(const message::Message& msg, const StatisticsOptions& options) {
    return epochDateTime(msg, options) + static_cast<eckit::Second>(options.step() * options.timeStep());
}

eckit::DateTime nextDateTime(const message::Message& msg, const StatisticsOptions& options) {
    return epochDateTime(msg, options) + static_cast<eckit::Second>((options.step() + 1) * options.timeStep());
}

eckit::DateTime winStartDateTime(const message::Message& msg, const StatisticsOptions& options) {
    return options.solver_send_initial_condition() ? currentDateTime(msg, options) : prevDateTime(msg, options);
}


DateTimePeriod::DateTimePeriod(StatisticsIO& IOmanager, const char* periodKind, const StatisticsOptions& options) :
    startPoint_{eckit::Date{0}, eckit::Time{0}}, endPoint_{eckit::Date{0}, eckit::Time{0}}, periodKind_{periodKind} {
    options.reader().startPeriod(partialPath, "period", "r");
    options.reader().readPeriod(startPoint_, endPoint_, creationPoint_);
    options.reader().endPeriod();
    return;
}

DateTimePeriod::DateTimePeriod(const eckit::DateTime& startPoint, const eckit::DateTime& creationPoint,
                               const eckit::DateTime& endPoint, const char* periodKind) :
    startPoint_{startPoint}, creationPoint_{creationPoint}, endPoint_{endPoint}, periodKind_{periodKind} {}


void DateTimePeriod::reset(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint) {
    startPoint_ = startPoint;
    creationPoint_ = startPoint;
    endPoint_ = endPoint;
}

bool DateTimePeriod::isWithin(const eckit::DateTime& dt) {
    if (creationPoint_ > dt) {
        std::ostringstream os;
        os << "startPoint : " << startPoint_ << " is outside of current period " << dt << std::endl;
        throw eckit::SeriousBug(os.str(), Here());
    }
    auto ret = (dt <= endPoint() + eckit::Second{1.0});
    LOG_DEBUG_LIB(LibMultio) << " ------ Is " << dt << " within " << *this << "? -- " << (ret ? "yes" : "no")
                             << std::endl;
    return ret;
}

long DateTimePeriod::timeSpanInSeconds() const {
    return long(endPoint_ - creationPoint_);
}


eckit::DateTime DateTimePeriod::startPoint() const {
    return startPoint_;
}

eckit::DateTime DateTimePeriod::endPoint() const {
    return endPoint_;
}

eckit::DateTime DateTimePeriod::creationPoint() const {
    return creationPoint_;
}


void DateTimePeriod::dump(StatisticsIO& IOmanager) const {
    options.dumper().startPeriod(partialPath, "period", "w");
    options.dumper().writePeriod(startPoint_, endPoint_, creationPoint_);
    options.dumper().endPeriod();
    /*
    // Read the file again to check the values
    eckit::DateTime start;
    eckit::DateTime end;
    eckit::DateTime creation;
    dumper.startPeriod( partialPath, "period", "r" );
    dumper.readPeriod( start, end, creation );
    dumper.endPeriod();
    if ( start != startPoint_) {
        throw eckit::SeriousBug("Error start point mismatch!", Here());
    };
    if ( end != endPoint_) {
        throw eckit::SeriousBug("Error end point mismatch!", Here());
    };
    if ( creation != creationPoint_ ) {
        throw eckit::SeriousBug("Error creation Point mismatch!", Here());
    };
    */
    return;
}

void DateTimePeriod::print(std::ostream& os) const {
    os << "Period(" << startPoint_ << " to " << endPoint() << ")";
}

std::ostream& operator<<(std::ostream& os, const DateTimePeriod& a) {
    a.print(os);
    return os;
}

}  // namespace action
}  // namespace multio
