
#include "MultioClient.h"

#include "eckit/config/LocalConfiguration.h"
#include "eckit/log/JSON.h"

#include "multio/LibMultio.h"
#include "multio/server/Message.h"
#include "multio/server/Transport.h"

namespace multio {
namespace server {

MultioClient::MultioClient(const eckit::Configuration& config) :
    clientCount_{config.getUnsigned("clientCount")},
    serverCount_{config.getUnsigned("serverCount")},
    transport_(createTransport(config)),
    serverPeers_{createServerPeers(config)} {}

// MultioClient::MultioClient(const eckit::Configuration& config, std::shared_ptr<Transport> trans) :
//     clientCount_{config.getUnsigned("clientCount")},
//     serverCount_{config.getUnsigned("serverCount")},
//     transport_{trans},
//     serverPeers_{createServerPeers(config)} {}

void MultioClient::openConnections() const {
    auto client = transport_->localPeer();
    for (auto& server : serverPeers_) {
        Message msg{Message::Header{Message::Tag::Open, client, *server}};
        transport_->send(msg);
    }
}

void MultioClient::closeConnections() const {
    auto client = transport_->localPeer();
    for (auto& server : serverPeers_) {
        Message msg{Message::Header{Message::Tag::Close, client, *server}};
        transport_->send(msg);
    }
}

void MultioClient::sendDomain(const std::string& name, const std::string& category,
                              eckit::Buffer&& domain) {
    Peer client = transport_->localPeer();
    for (auto& server : serverPeers_) {
        Message msg{
            Message::Header{Message::Tag::Domain, client, *server, name, category, clientCount_},
            std::move(domain)};

        transport_->send(msg);
    }
}

void MultioClient::sendField(const std::string& name, const std::string& category, size_t gl_size,
                             const std::string& domain, const Metadata& metadata,
                             eckit::Buffer&& field) {
    Peer client = transport_->localPeer();

    std::stringstream field_id;
    eckit::JSON json(field_id);
    json << metadata;

    // Choose server
    auto id = std::hash<std::string>{}(field_id.str()) % serverCount_;
    ASSERT(id < serverPeers_.size());

    Message msg{Message::Header{Message::Tag::Field, client, *serverPeers_[id], name, category,
                                clientCount_, gl_size, domain, field_id.str()},
                std::move(field)};

    transport_->send(msg);
}

Transport* MultioClient::createTransport(const eckit::Configuration& config) {
    return TransportFactory::instance().build(config.getString("transport"), config);
}

MultioClient::PeerList MultioClient::createServerPeers(const eckit::Configuration& config) {
    PeerList serverPeers;

    std::string transport = config.getString("transport");
    std::string group = config.getString("group");

    eckit::Log::debug<multio::LibMultio>()
        << "transport = " << transport << ", group = " << group << ", clients = " << clientCount_
        << ", servers = " << serverCount_ << std::endl;

    // For MPI
    if (transport == "mpi") {
        auto comm_size = clientCount_ + serverCount_;
        auto rank = clientCount_;
        while (rank != comm_size) {
            serverPeers.emplace_back(new MpiPeer{group, rank++});
        }

        eckit::Log::debug<multio::LibMultio>()
            << "Returning list of " << serverPeers.size() << " server"
            << (serverPeers.size() > 1 ? "s" : "") << std::endl;

        return serverPeers;
    }

    // For TCP
    for (auto cfg : config.getSubConfigurations("servers")) {
        auto host = cfg.getString("host");
        for (auto port : cfg.getUnsignedVector("ports")) {
            serverPeers.emplace_back(new TcpPeer{host, port});
        }
    }
    return serverPeers;
}

}
}