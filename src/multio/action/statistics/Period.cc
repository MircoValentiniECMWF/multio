
#include "Period.h"

#include <fstream>
#include <iostream>

#include "eckit/filesystem/PathName.h"
#include "multio/LibMultio.h"

namespace multio {
namespace action {
DateTimePeriod::DateTimePeriod(const std::string& partialPath, const StatisticsOptions& options) :
    startPoint_{eckit::Date{0}, eckit::Time{0}}, endPoint_{eckit::Date{0}, eckit::Time{0}}, offset_{0} {
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
    wf.read((char*)&of, sizeof(long));
    wf.read((char*)&cs, sizeof(long));
    wf.close();
    long checksum = 0;
    checksum ^= sd;
    checksum ^= st;
    checksum ^= ed;
    checksum ^= et;
    checksum ^= of;
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
    offset_ = of;
    return;
}

DateTimePeriod::DateTimePeriod(const eckit::DateTime& startPoint, eckit::Second duration, long offset) :
    startPoint_{startPoint}, endPoint_{startPoint_ + duration}, offset_{offset} {}

DateTimePeriod::DateTimePeriod(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint, long offset) :
    startPoint_{startPoint}, endPoint_{endPoint}, offset_{offset} {}

DateTimePeriod::DateTimePeriod(const eckit::DateTime& startPoint, eckit::Second duration) :
    DateTimePeriod(startPoint, duration, 0L) {}

DateTimePeriod::DateTimePeriod(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint) :
    DateTimePeriod(startPoint, endPoint, 0L) {}


void DateTimePeriod::reset(const eckit::DateTime& current) {
    auto duration = endPoint_ - startPoint_;
    startPoint_ = current;
    offset_ = 0;
    endPoint_ = startPoint_ + duration;
}

void DateTimePeriod::reset(const eckit::DateTime& startPoint, const eckit::DateTime& endPoint) {
    offset_ = 0;
    startPoint_ = startPoint;
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
    return long(endPoint_ - startPoint_ + offset_);
}

eckit::DateTime DateTimePeriod::startPoint() const {
    return startPoint_;
}

eckit::DateTime DateTimePeriod::endPoint() const {
    return endPoint_;
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
    checksum ^= offset_;
    wf.write((char*)&sd, sizeof(long));
    wf.write((char*)&st, sizeof(long));
    wf.write((char*)&ed, sizeof(long));
    wf.write((char*)&et, sizeof(long));
    wf.write((char*)&offset_, sizeof(long));
    wf.write((char*)&checksum, sizeof(long));
    wf.close();
    if (!wf.good()) {
        throw eckit::SeriousBug("Error occurred at writing time!", Here());
    }
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
