/*
 * (C) Copyright 1996-2015 ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Tiago Quintino
/// @author Simon Smart
/// @date Dec 2015

#include <iosfwd>
#include <fstream>

#include "multio/FileSink.h"
#include "multio/DataSink.h"

#include "eckit/exception/Exceptions.h"
#include "eckit/io/Length.h"
#include "eckit/thread/AutoLock.h"
#include "eckit/thread/Mutex.h"

using namespace eckit;

//----------------------------------------------------------------------------------------------------------------------

namespace multio {

FileSink::FileSink(const Configuration& config) : DataSink() {}

FileSink::~FileSink() {
    if (is_open())
        close();
}

void FileSink::open(const std::string& key) {
    eckit::Log::info() << "[" << *this << "]: open" << std::endl;

    eckit::AutoLock<eckit::Mutex> lock(file_mutex_);

    if (is_open())
        throw eckit::SeriousBug("FileSink: Cannot open multiple times");

    key_ = key;
    file_.open(key_.c_str(), std::ios_base::trunc | std::ios_base::out);
}

void FileSink::write(const void* buffer, const Length& length) {
    eckit::Log::info() << "[" << *this << "]: write (" << length << ")" << std::endl;

    eckit::AutoLock<eckit::Mutex> lock(file_mutex_);

    if (!is_open())
        throw eckit::SeriousBug(std::string("FileSink: Cannot write without opening"));

    file_.write(reinterpret_cast<const char*>(buffer), length);
}

void FileSink::close() {
    eckit::Log::info() << "[" << *this << "]: close" << std::endl;

    eckit::AutoLock<eckit::Mutex> lock(file_mutex_);

    file_.close();
    key_ = "";
}

bool FileSink::is_open() const {
    return file_.is_open();
}

void FileSink::print(std::ostream& os) const {
    os << "DataSink (FileSink): " << key_;
}

DataSinkBuilder<FileSink> FileSinkFactorySingleton("foo");

}  // namespace multiplexer


