/// @author Domokos Sarmany

/// @date Jan 2022

#ifndef multio_server_transport_TransportRegistry_H
#define multio_server_transport_TransportRegistry_H

#include "multio/transport/Transport.h" // This means circular dependency at the minute

namespace eckit { class Configuration; }

namespace multio {
namespace transport {

using message::Message;

class TransportRegistry : public eckit::NonCopyable {
public:
    static TransportRegistry& instance();

    std::shared_ptr<transport::Transport> get(const eckit::Configuration& config);

    void openConnections();
    void closeConnections();

    void abort(const std::string& serverName);
    void abortAll();

private:
    void add(const std::string& serverName);

    std::map<std::string, std::shared_ptr<transport::Transport>> transports_;
    std::mutex mutex_;
};

}  // namespace action
}  // namespace multio

#endif
