#include "AtlasIO.h"

#include <cstdio>
#include <iomanip>

#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"
#include "multio/LibMultio.h"


namespace multio::action {

AtlasIO::AtlasIO(const std::string& path, const std::string& prefix) : StatisticsIO{path, prefix, "atlasIO"} {};

void AtlasIO::write(const std::string& name, std::size_t writeSize) {
    LOG_DEBUG_LIB(LibMultio) << " - The name of the window write file is :: " << generateCurrFileName(name)
                             << std::endl;
    removeCurrFile(name);
    const std::string fname = generateCurrFileName(name);
    atlas::io::RecordWriter record;
    record.set(name, buffer, no_compression);
    record.write(fname);
    removePrevFile(name, 2);
    return;
};

void AtlasIO::read(const std::string& name, std::size_t writeSize) {
    LOG_DEBUG_LIB(LibMultio) << " - The name of the operation read file is :: " << generateFileName(name, 0)
                             << std::endl;
    const std::string fname = generateCurrFileName(name);
    atlas::io::RecordReader record(fname);
    record.read(name, buffer_).wait();
    return;
};

void AtlasIO::flush() {
    // TODO: Decide what to do when flush is called. Flush partial statistics when the Tag::Flush is received is
    // probably okay
    return;
};

void AtlasIO::checkFileExist(const std::string& name) const {
    eckit::PathName file{name};
    if (!file.exists()) {
        std::ostringstream os;
        os << "ERROR : wrong file not exist : (" << name << ")";
        throw eckit::SeriousBug{os.str(), Here()};
    }
    return;
};

void AtlasIO::checkFileSize(const std::string& name, size_t expectedSize) const {
    eckit::PathName file{name};
    if (file.size() != expectedSize) {
        std::ostringstream os;
        os << "ERROR : wrong file size for restart : (" << name << ")";
        throw eckit::SeriousBug{os.str(), Here()};
    }
    return;
};

StatisticsIOBuilder<AtlasIO> AtalsIOBuilder("atlas_io");

}  // namespace multio::action
