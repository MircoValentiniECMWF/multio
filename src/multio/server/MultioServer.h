/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#pragma once

#include <memory>

#include "multio/config/ComponentConfiguration.h"
#include "multio/server/ServerRuntime.h"
#include "multio/util/FailureHandling.h"

namespace eckit {
class LocalConfiguration;
}  // namespace eckit

namespace multio {

using config::MultioConfiguration;
using config::MultioConfigurationHolder;
using util::FailureAware;

namespace transport {
class Transport;
}

namespace server {

struct ServerFailureTraits {
    using OnErrorType = util::OnServerError;
    using FailureOptions = util::DefaultFailureOptions;
    using FailureState = util::DefaultFailureState;
    using TagSequence = util::integer_sequence<OnErrorType, OnErrorType::Propagate, OnErrorType::Recover,
                                               OnErrorType::AbortTransport>;
    static inline std::optional<OnErrorType> parse(const std::string& str) {
        return util::parseErrorTag<OnErrorType, TagSequence>(str);
    }
    static inline OnErrorType defaultOnErrorTag() { return OnErrorType::Propagate; };
    static inline FailureOptions parseFailureOptions(const eckit::Configuration& conf) {
        return util::parseDefaultFailureOptions(conf);
    };
    static inline std::string configKey() { return std::string("on-error"); };
    static inline std::string componentName() { return std::string("Server"); };
};


namespace detail {
// Owns the server state before FailureAware is constructed (FailureAware needs the configuration)
struct SharedHolder {
    explicit SharedHolder(MultioConfiguration&& multioConf) :
        shared_{std::make_unique<Shared>(std::move(multioConf))} {}
    std::unique_ptr<Shared> shared_;
};
}  // namespace detail

/// Server running the runnables of the server runtime (ServerRuntime.h) on its own threads, one per runnable.
/// The constructor returns when the server has finished.
class MultioServer : private detail::SharedHolder, public FailureAware<ServerFailureTraits> {
public:
    MultioServer(MultioConfiguration&& multioConf);
    ~MultioServer();

    util::FailureHandlerResponse handleFailure(util::OnServerError, const util::FailureContext&,
                                               util::DefaultFailureState&) const override;

private:
    void run();
};
}  // namespace server
}  // namespace multio
