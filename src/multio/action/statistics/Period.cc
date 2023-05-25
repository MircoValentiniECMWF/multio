
#include "Period.h"

#include <fstream>
#include <iostream>

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


DateTimePeriod::DateTimePeriod(const std::string& partialPath, const char* periodKind,
                               const StatisticsOptions& options) :
    startPoint_{eckit::Date{0}, eckit::Time{0}}, endPoint_{eckit::Date{0}, eckit::Time{0}}, periodKind_{periodKind} {
    long sd;
    long st;
    long ed;
    long et;
    long of;
    long cs;
    // std::ostringstream tmpOs;
    std::ostringstream defOs;
    defOs << partialPath << "-" << std::to_string(options.restartStep()) << "-period.bin";
    // eckit::PathName tmpFile(tmpOs.str());
    // eckit::PathName defFile(defOs.str());
    std::string fname = defOs.str();
    std::ifstream wf(fname, std::ios::binary);
    if (!wf) {
        std::ostringstream err;
        err << "Cannot open file :: " << fname;
        throw eckit::SeriousBug(err.str(), Here());
    }
    wf.read((char*)&sd, sizeof(long));
    wf.read((char*)&st, sizeof(long));
    wf.read((char*)&ed, sizeof(long));
    wf.read((char*)&et, sizeof(long));
    wf.read((char*)&cs, sizeof(long));
    wf.close();
    long checksum = 0;
    checksum ^= sd;
    checksum ^= st;
    checksum ^= ed;
    checksum ^= et;
    if (!wf.good()) {
        std::ostringstream err;
        err << "Error occurred at writing time :: " << fname;
        throw eckit::SeriousBug(err.str(), Here());
    }
    if (cs != checksum) {
        std::ostringstream err;
        err << "Error checksum not correct :: " << cs << ", " << checksum;
        throw eckit::SeriousBug(err.str(), Here());
    }
    startPoint_ = eckit::DateTime{eckit::Date{sd}, eckit::Time{st}};
    endPoint_ = eckit::DateTime{eckit::Date{ed}, eckit::Time{et}};
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
    if (startPoint_ > dt) {
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
    // The offset is added to fix the first timestep when no "step 0"
    // is present. Better way to handle this is through a redesign of the action
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


void DateTimePeriod::dump(const std::string& partialPath, const long step) const {
    std::ostringstream tmpOs;
    std::ostringstream defOs;
    std::ostringstream oldOs;
    tmpOs << partialPath << "-" << std::to_string(step) << "-period.tmp.bin";
    defOs << partialPath << "-" << std::to_string(step) << "-period.bin";
    oldOs << partialPath << "-" << std::to_string(step - 2) << "-period.bin";
    if (eckit::PathName oldFile(oldOs.str()); oldFile.exists()) {
        oldFile.unlink();
    }
    eckit::PathName tmpFile(tmpOs.str());
    eckit::PathName defFile(defOs.str());
    std::string fname = tmpOs.str();
    std::ofstream wf(fname, std::ios::binary);
    if (!wf) {
        throw eckit::SeriousBug("Cannot open file!", Here());
    }
    long dim;
    long sd = startPoint_.date().yyyymmdd();
    long st = startPoint_.time().hhmmss();
    long ed = endPoint_.date().yyyymmdd();
    long et = endPoint_.time().hhmmss();
    long checksum = 0;
    checksum ^= sd;
    checksum ^= st;
    checksum ^= ed;
    checksum ^= et;
    wf.write((char*)&sd, sizeof(long));
    wf.write((char*)&st, sizeof(long));
    wf.write((char*)&ed, sizeof(long));
    wf.write((char*)&et, sizeof(long));
    wf.write((char*)&checksum, sizeof(long));
    wf.close();
    if (!wf.good()) {
        throw eckit::SeriousBug("Error occurred at writing time!", Here());
    }
    atlas::io::RecordWriter record;
    record.set("startDate", atlas::io::ref(sd), no_compression);
    record.set("startTime", atlas::io::ref(st), no_compression);
    record.set("endDate", atlas::io::ref(ed), no_compression);
    record.set("endTime", atlas::io::ref(et), no_compression);
    record.write(fname+".atlas_io");
    eckit::PathName::rename(tmpFile, defFile);
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
