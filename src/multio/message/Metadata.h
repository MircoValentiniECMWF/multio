/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @author Domokos Sarmany
/// @author Simon Smart
/// @author Tiago Quintino

/// @date Oct 2019

#ifndef multio_server_Metadata_H
#define multio_server_Metadata_H

#include "eckit/config/LocalConfiguration.h"

namespace multio {
namespace message {

using Metadata = eckit::LocalConfiguration;

std::string to_string(const Metadata& metadata);
Metadata to_metadata(const std::string& fieldId);

}  // namespace message
}  // namespace multio

#endif
