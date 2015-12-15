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


#ifndef multiplexer_MultiplexerSink_H
#define multiplexer_MultiplexerSink_H

#include <iosfwd>
#include <string>
#include <vector>

#include "eckit/io/Length.h"
#include "eckit/memory/NonCopyable.h"
#include "eckit/memory/ScopedPtr.h"
#include "eckit/thread/Mutex.h"
#include "multiplexer/DataSink.h"

namespace eckit {
namespace multiplexer {

//----------------------------------------------------------------------------------------------------------------------

class MultiplexerSink : public DataSink {

public: // types

    typedef std::vector<DataSink*> sink_store_t;

public: // methods

    MultiplexerSink(const Configuration& config);

    virtual ~MultiplexerSink();

    virtual void open(const std::string& key);

    virtual void write(const void* buffer, const Length& length);

    virtual void close();

protected: // methods

    virtual void print(std::ostream&) const;

protected: // members

    sink_store_t sinks_;

};

//----------------------------------------------------------------------------------------------------------------------

}  // namespace multiplexer
}  // namespace eckit

#endif // multiplexer_MultiplexerSink_H

