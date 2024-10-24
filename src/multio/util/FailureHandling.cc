
#include "FailureHandling.h"

using namespace multio::util;

std::string eckit::Translator<OnClientError, std::string>::operator()(OnClientError tag) {
    switch (tag) {
        case OnClientError::Propagate:
            return std::string("propagate");
        case OnClientError::Recover:
            return std::string("recover");
        case OnClientError::AbortAllTransports:
            return std::string("abort-all-transports");
    }
}

std::string eckit::Translator<OnServerError, std::string>::operator()(OnServerError tag) {
    switch (tag) {
        case OnServerError::Propagate:
            return std::string("propagate");
        case OnServerError::Recover:
            return std::string("recover");
        case OnServerError::AbortTransport:
            return std::string("abort-transport");
    }
}

std::string eckit::Translator<OnPlanError, std::string>::operator()(OnPlanError tag) {
    switch (tag) {
        case OnPlanError::Propagate:
            return std::string("propagate");
        case OnPlanError::Recover:
            return std::string("recover");
    }
}

std::string eckit::Translator<OnActionError, std::string>::operator()(OnActionError tag) {
    switch (tag) {
        case OnActionError::Propagate:
            return std::string("propagate");
        case OnActionError::Recover:
            return std::string("recover");
    }
}

std::string eckit::Translator<OnTransportError, std::string>::operator()(OnTransportError tag) {
    switch (tag) {
        case OnTransportError::Propagate:
            return std::string("propagate");
        case OnTransportError::Recover:
            return std::string("recover");
    }
}

std::string eckit::Translator<OnReceiveError, std::string>::operator()(OnReceiveError tag) {
    switch (tag) {
        case OnReceiveError::Propagate:
            return std::string("propagate");
    }
}

std::string eckit::Translator<OnDispatchError, std::string>::operator()(OnDispatchError tag) {
    switch (tag) {
        case OnDispatchError::Propagate:
            return std::string("propagate");
    }
}

namespace multio {
namespace util {

}  // namespace util
}  // namespace multio
