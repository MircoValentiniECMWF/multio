#pragma once

#include <mutex>
#include <optional>
#include <queue>
#include <string>
#include <tuple>
#include <vector>

#include "eckit/container/Queue.h"
#include "eckit/io/Buffer.h"
#include "eckit/log/Statistics.h"
#include "eckit/mpi/Comm.h"
#include "eckit/mpi/Group.h"
#include "eckit/serialisation/ResizableMemoryStream.h"

#include "multio/transport/StreamPool.h"
#include "multio/transport/Transport.h"

namespace multio::transport {

struct ReceivedBuffer {
    MpiBuffer* buffer;
    size_t size;
};





using MpiPeerSetup = std::tuple<MpiPeer, eckit::mpi::Group, eckit::mpi::Group, eckit::mpi::Group>;

class MpiXTransport final : public Transport {
public:
    explicit MpiXTransport(const ComponentConfiguration& compConf);

    // Transport on the communicators registered as baseName in the MultioConfiguration
    // (one transport per client group on a server with several client groups)
    MpiXTransport(const ComponentConfiguration& compConf, const std::string& baseName);

    ~MpiXTransport();

    // Server side, without internal buffering: receive the next pending MPI message (if any)
    // into the given buffer. Returns false if no message is pending.
    bool tryReceive(eckit::Buffer& target, size_t& size);

    // Decode the messages contained in a received buffer
    static void decodeBuffer(eckit::Buffer& buffer, size_t size, std::vector<Message>& messages);

    void synchronize() override;

private:
    MpiXTransport(const ComponentConfiguration& compConf, MpiPeerSetup&& peerSetup);

    void openConnections() override;
    void closeConnections() override;

    Message receive() override;

    void abort(std::exception_ptr) override;

    void send(const Message& msg) override;

    void bufferedSend(const Message& msg) override;

    void createPeers() const override;

    void print(std::ostream& os) const override;

    const Peer& localPeer() const override;

    void listen() override;

    PeerList createServerPeers() const override;

    const eckit::mpi::Comm& comm() const;

    eckit::mpi::Status probe();
    size_t blockingReceive(eckit::mpi::Status& status, MpiBuffer& buffer);

    void encodeMessage(eckit::Stream& strm, const Message& msg);

    size_t getMpiPoolSize(const ComponentConfiguration& compConf);
    size_t getMpiBufferSize(const ComponentConfiguration& compConf);

    MpiPeer local_;
    eckit::mpi::Group parentGroup_;
    eckit::mpi::Group clientGroup_;
    eckit::mpi::Group serverGroup_;

    // Buffer pool, created on first use: sending (client) and the internal receive path of
    // listen()/receive(). Not created by the server runtime, which receives with tryReceive.
    StreamPool& pool();
    std::optional<StreamPool> pool_;
    std::once_flag poolOnce_;

    eckit::Queue<ReceivedBuffer> streamQueue_;

    std::queue<Message> msgPack_;
};

}  // namespace multio::transport