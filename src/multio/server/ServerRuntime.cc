/*
 * (C) Copyright 2026- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include "ServerRuntime.h"

#include <iomanip>
#include <set>
#include <thread>

#include "eckit/utils/Translator.h"

#include "multio/LibMultio.h"
#include "multio/action/Plan.h"
#include "multio/config/ComponentConfiguration.h"
#include "multio/domain/Mappings.h"
#include "multio/domain/Mask.h"
#include "multio/message/Parametrization.h"
#include "multio/transport/MpiXTransport.h"
#include "multio/transport/Transport.h"
#include "multio/util/Environment.h"

namespace multio::server {

using message::Message;

namespace {

constexpr std::size_t MiB = 1024 * 1024;

eckit::LocalConfiguration getServerConf(const config::MultioConfiguration& multioConf) {
    if (multioConf.parsedConfig().has("server")) {
        return multioConf.parsedConfig().getSubConfiguration("server");
    }
    std::ostringstream oss;
    oss << "Configuration 'server' not found in configuration file " << multioConf.configFile();
    throw eckit::UserError(oss.str(), Here());
}

std::size_t envOr(const char* name, std::size_t value) {
    auto env = util::getEnv(name);
    if (env) {
        return eckit::translate<std::size_t>(std::string{*env});
    }
    return value;
}

std::size_t sizeOr(const eckit::LocalConfiguration& conf, const std::string& key, std::size_t value) {
    return conf.has(key) ? static_cast<std::size_t>(conf.getLong(key)) : value;
}

double secondsOr(const eckit::LocalConfiguration& conf, const std::string& key, double value) {
    return conf.has(key) ? conf.getDouble(key) : value;
}

}  // namespace

//----------------------------------------------------------------------------------------------------------------------

std::ostream& operator<<(std::ostream& os, const QueueUsage& u) {
    const auto flags = os.flags();
    const auto precision = os.precision();
    os << std::left << std::setw(36) << u.name << " count=" << u.count << "/" << u.capacityCount
       << " (max " << u.maxCount << ")"
       << " MiB=" << std::fixed << std::setprecision(1) << static_cast<double>(u.bytes) / MiB;
    if (u.capacityBytes > 0) {
        os << "/" << static_cast<double>(u.capacityBytes) / MiB;
    }
    os << " (max " << static_cast<double>(u.maxBytes) / MiB << ")"
       << " total=" << u.total << " producer-wait=" << std::setprecision(2) << u.producerWaitSeconds << "s"
       << " consumer-wait=" << u.consumerWaitSeconds << "s";
    os.flags(flags);
    os.precision(precision);
    return os;
}

//----------------------------------------------------------------------------------------------------------------------

ReceiveBufferPool::ReceiveBufferPool(std::size_t count, std::size_t bufferSize, const std::atomic<bool>& stop,
                                     double warnAfterSeconds) :
    bufferSize_{bufferSize}, stop_{stop}, warnAfter_{warnAfterSeconds} {
    ASSERT(count > 0);
    for (std::size_t i = 0; i < count; ++i) {
        buffers_.emplace_back(std::make_unique<eckit::Buffer>(bufferSize));
        free_.push_back(static_cast<long>(count - 1 - i));
    }
}

long ReceiveBufferPool::acquire() {
    std::unique_lock<std::mutex> lock{mutex_};
    const auto start = std::chrono::steady_clock::now();
    bool warned = false;
    while (free_.empty() && !stop_.load()) {
        released_.wait_for(lock, std::chrono::seconds(1));
        const double waited = std::chrono::duration<double>(std::chrono::steady_clock::now() - start).count();
        if (!warned && warnAfter_ > 0 && waited > warnAfter_) {
            warned = true;
            eckit::Log::warning() << "multio server: listener waiting for " << waited
                                  << " s for a free receive buffer (all " << buffers_.size()
                                  << " in use: receivers or dispatcher are slow)" << std::endl;
        }
    }
    waited_ += std::chrono::duration<double>(std::chrono::steady_clock::now() - start).count();
    if (stop_.load()) {
        return -1;
    }
    long index = free_.back();
    free_.pop_back();
    ++acquired_;
    maxInUse_ = std::max(maxInUse_, buffers_.size() - free_.size());
    return index;
}

void ReceiveBufferPool::release(long index) {
    std::lock_guard<std::mutex> lock{mutex_};
    ASSERT(index >= 0 && static_cast<std::size_t>(index) < buffers_.size());
    free_.push_back(index);
    released_.notify_one();
}

eckit::Buffer& ReceiveBufferPool::buffer(long index) {
    ASSERT(index >= 0 && static_cast<std::size_t>(index) < buffers_.size());
    return *buffers_[index];
}

void ReceiveBufferPool::wakeAll() {
    std::lock_guard<std::mutex> lock{mutex_};
    released_.notify_all();
}

QueueUsage ReceiveBufferPool::usage() const {
    std::lock_guard<std::mutex> lock{mutex_};
    QueueUsage u;
    u.name = "receive-buffers";
    u.count = buffers_.size() - free_.size();
    u.bytes = u.count * bufferSize_;
    u.maxCount = maxInUse_;
    u.maxBytes = maxInUse_ * bufferSize_;
    u.capacityCount = buffers_.size();
    u.capacityBytes = buffers_.size() * bufferSize_;
    u.total = acquired_;
    u.producerWaitSeconds = waited_;
    return u;
}

//----------------------------------------------------------------------------------------------------------------------
// Runnables
//----------------------------------------------------------------------------------------------------------------------

void Runnable::init(Shared& shared) {
    if (state_ != State::Created) {
        throw eckit::SeriousBug("multio server runnable " + name_ + ": init called twice", Here());
    }
    shared_ = &shared;
    state_ = State::Initialised;
}

int Runnable::run(const util::DrHook& drHook) {
    if (state_ != State::Initialised) {
        error_ = "multio server runnable " + name_ + ": run called before init or twice";
        return 1;
    }
    drHook_ = drHook;
    try {
        setup();
        while (step()) {
        }
        teardown();
        state_ = State::Finished;
        if (notifiesCoordinator()) {
            shared_->mailbox().push(RunnableEvent{name_, false, ""});
        }
        return 0;
    }
    catch (const std::exception& e) {
        error_ = e.what();
    }
    catch (...) {
        error_ = "unknown exception";
    }
    state_ = State::Finished;
    eckit::Log::error() << "multio server runnable " << name_ << " failed: " << error_ << std::endl;
    if (notifiesCoordinator()) {
        shared_->stop();
        shared_->mailbox().push(RunnableEvent{name_, true, error_});
    }
    return 1;
}

void Runnable::free() {
    state_ = State::Freed;
    shared_ = nullptr;
}

Shared& Runnable::shared() {
    ASSERT(shared_ != nullptr);
    return *shared_;
}

namespace {

//----------------------------------------------------------------------------------------------------------------------

/// Receives the MPI messages of all the client groups into the receive buffers (mpix), or drives the
/// internal receive loop of the transport (other transports, single group)
class ListenerRunnable final : public Runnable {
public:
    ListenerRunnable() : Runnable{"listener"} {}
    std::string kind() const override { return "listener"; }

protected:
    bool step() override {
        auto& s = shared();
        if (s.stopping() || allFinished()) {
            releaseBuffer();
            return false;
        }
        auto* pool = s.receiveBuffers();
        if (!pool) {
            s.groups().front()->transport->listen();
            std::this_thread::yield();
            return true;
        }
        bool received = false;
        for (auto& group : s.groups()) {
            if (group->finished.load()) {
                continue;
            }
            if (buffer_ < 0) {
                buffer_ = pool->acquire();
                if (buffer_ < 0) {
                    return false;
                }
            }
            std::size_t size = 0;
            if (group->mpix->tryReceive(pool->buffer(buffer_), size)) {
                received = true;
                if (group->chunks->push(ReceivedChunk{buffer_, size}, size)) {
                    buffer_ = -1;
                }
                else if (s.stopping()) {
                    return false;
                }
            }
        }
        if (!received) {
            std::this_thread::yield();
        }
        return true;
    }

    void teardown() override { releaseBuffer(); }

private:
    bool allFinished() {
        for (auto& group : shared().groups()) {
            if (!group->finished.load()) {
                return false;
            }
        }
        return true;
    }

    void releaseBuffer() {
        if (buffer_ >= 0) {
            shared().receiveBuffers()->release(buffer_);
            buffer_ = -1;
        }
    }

    long buffer_ = -1;
};

//----------------------------------------------------------------------------------------------------------------------

/// Decodes the messages of one client group and keeps track of its connections
class ReceiverRunnable final : public Runnable {
public:
    explicit ReceiverRunnable(std::size_t groupIndex, const std::string& groupName) :
        Runnable{"receiver-" + groupName}, groupIndex_{groupIndex} {}
    std::string kind() const override { return "receiver"; }

protected:
    void setup() override { clientCount_ = group().transport->clientCount(); }

    bool step() override {
        auto& s = shared();
        if (done() || s.stopping()) {
            return finish();
        }
        auto& g = group();
        if (g.mpix) {
            ReceivedChunk chunk;
            if (!g.chunks->pop(chunk)) {
                return finish();
            }
            decoded_.clear();
            transport::MpiXTransport::decodeBuffer(s.receiveBuffers()->buffer(chunk.buffer), chunk.size, decoded_);
            s.receiveBuffers()->release(chunk.buffer);
            for (auto& msg : decoded_) {
                if (!handle(std::move(msg))) {
                    return finish();
                }
            }
            decoded_.clear();
        }
        else {
            if (!handle(g.transport->receive())) {
                return finish();
            }
        }
        return true;
    }

private:
    Shared::ClientGroup& group() { return *shared().groups()[groupIndex_]; }

    bool done() const { return connections_.empty() && openedCount_ == clientCount_; }

    bool finish() {
        if (!finished_) {
            finished_ = true;
            auto& g = group();
            g.finished.store(true);
            if (g.chunks) {
                g.chunks->close();
            }
            shared().receiverFinished();
        }
        return false;
    }

    void checkConnection(const message::Peer& conn) const {
        if (connections_.find(conn) == connections_.end()) {
            std::ostringstream oss;
            oss << "Connection to " << conn << " is not open";
            throw eckit::SeriousBug(oss.str(), Here());
        }
    }

    bool handle(Message msg) {
        switch (msg.tag()) {
            case Message::Tag::Open:
                connections_.insert(msg.source());
                ++openedCount_;
                return true;
            case Message::Tag::Close: {
                auto it = connections_.find(msg.source());
                if (it == connections_.end()) {
                    std::ostringstream oss;
                    oss << "Close from " << msg.source() << ", connection is not open";
                    throw eckit::SeriousBug(oss.str(), Here());
                }
                connections_.erase(it);
                return true;
            }
            case Message::Tag::Synchronization:
                // Synchronizations are counted per client group
                checkConnection(msg.source());
                if (++syncCount_ == clientCount_) {
                    syncCount_ = 0;
                    return shared().messages().push(std::move(msg), 0);
                }
                return true;
            case Message::Tag::Domain:
            case Message::Tag::Parametrization:
            case Message::Tag::Mask:
            case Message::Tag::Notification:
            case Message::Tag::Flush:
            case Message::Tag::Field: {
                checkConnection(msg.source());
                const std::size_t bytes = msg.size();
                return shared().messages().push(std::move(msg), bytes);
            }
            default: {
                std::ostringstream oss;
                oss << "Unhandled message: " << msg;
                throw eckit::SeriousBug(oss.str(), Here());
            }
        }
    }

    std::size_t groupIndex_;
    std::size_t clientCount_ = 0;
    std::size_t openedCount_ = 0;
    std::size_t syncCount_ = 0;
    bool finished_ = false;
    std::set<message::Peer> connections_;
    std::vector<Message> decoded_;
};

//----------------------------------------------------------------------------------------------------------------------

/// Processes the messages through the action plans
class DispatcherRunnable final : public Runnable {
public:
    DispatcherRunnable() : Runnable{"dispatcher"} {}
    std::string kind() const override { return "dispatcher"; }

protected:
    void setup() override {
        auto& s = shared();
        plans_ = action::Plan::makePlans(s.serverConfig().getSubConfigurations("plans"), s.configuration());
    }

    bool step() override {
        Message msg;
        if (!shared().messages().pop(msg)) {
            return false;
        }
        switch (msg.tag()) {
            case Message::Tag::Domain:
                domain::Mappings::instance().add(std::move(msg));
                break;
            case Message::Tag::Mask:
                domain::Mask::instance().add(std::move(msg));
                break;
            case Message::Tag::Parametrization:
                message::Parametrization::instance().update(std::move(msg));
                break;
            case Message::Tag::Synchronization:
                shared().transportOf(msg.source()).synchronize();
                break;
            default:
                for (const auto& plan : plans_) {
                    plan->process(msg);
                }
        }
        return true;
    }

private:
    std::vector<std::unique_ptr<action::Plan>> plans_;
};

//----------------------------------------------------------------------------------------------------------------------

/// Waits for the termination of the other runnables, stops the server on errors, reports the memory usage
class CoordinatorRunnable final : public Runnable {
public:
    explicit CoordinatorRunnable(std::size_t expected) : Runnable{"coordinator"}, expected_{expected} {}
    std::string kind() const override { return "coordinator"; }

protected:
    bool notifiesCoordinator() const override { return false; }

    void setup() override { lastLog_ = std::chrono::steady_clock::now(); }

    bool step() override {
        auto& s = shared();
        if (finished_ >= expected_) {
            eckit::Log::info() << "multio server: memory usage at the end of the run" << std::endl;
            s.printUsage(eckit::Log::info());
            if (!errors_.empty()) {
                throw eckit::SeriousBug("multio server failed:\n" + errors_, Here());
            }
            return false;
        }
        const double period = s.usageLogSeconds();
        RunnableEvent event;
        auto result = s.mailbox().popFor(event, period > 0 ? period : 1.0);
        if (result == BoundedQueue<RunnableEvent>::PopResult::Element) {
            ++finished_;
            if (event.error) {
                errors_ += "  " + event.runnable + ": " + event.what + "\n";
                s.stop();
            }
        }
        if (period > 0
            && std::chrono::duration<double>(std::chrono::steady_clock::now() - lastLog_).count() >= period) {
            lastLog_ = std::chrono::steady_clock::now();
            s.printUsage(eckit::Log::info());
        }
        return true;
    }

private:
    std::size_t expected_;
    std::size_t finished_ = 0;
    std::string errors_;
    std::chrono::steady_clock::time_point lastLog_;
};

}  // namespace

//----------------------------------------------------------------------------------------------------------------------
// Shared
//----------------------------------------------------------------------------------------------------------------------

Shared::Shared(config::MultioConfiguration&& multioConf) :
    config::MultioConfigurationHolder(std::move(multioConf), config::LocalPeerTag::Server),
    serverConf_{getServerConf(multioConfig())} {

    // Memory limits (server configuration, with the old environment variables as defaults)
    const std::size_t bufferCount = sizeOr(serverConf_, "receive-buffers", 4);
    const std::size_t bufferSize
        = sizeOr(serverConf_, "receive-buffer-size",
                 envOr("MULTIO_SERVER_MPI_BUFFER_SIZE", envOr("MULTIO_MPI_BUFFER_SIZE", 64 * MiB)));
    const std::size_t messageBytes = sizeOr(serverConf_, "message-queue-bytes", 4096 * MiB);
    const std::size_t messageCount
        = sizeOr(serverConf_, "message-queue-size", envOr("MULTIO_MESSAGE_QUEUE_SIZE", 1024 * 1024));
    const double warnAfter = secondsOr(serverConf_, "queue-warning-seconds", 60);
    const double abortAfter = secondsOr(serverConf_, "queue-abort-seconds", 0);
    usageLogSeconds_ = secondsOr(serverConf_, "usage-log-seconds", 0);

    const std::string transportName = serverConf_.getString("transport");
    const auto& conf = multioConfig();
    config::ComponentConfiguration compConf{serverConf_, conf};

    if (transportName == "mpix" && conf.getCommunicatorCount() > 0) {
        // One transport per client group; data received into the buffers of the pool
        receiveBuffers_ = std::make_unique<ReceiveBufferPool>(bufferCount, bufferSize, stop_, warnAfter);
        for (std::size_t i = 0; i < conf.getCommunicatorCount(); ++i) {
            auto group = std::make_unique<ClientGroup>();
            group->name = conf.getCommunicatorNameById(static_cast<int>(i));
            auto transport = std::make_unique<transport::MpiXTransport>(compConf, group->name);
            group->mpix = transport.get();
            group->transport = std::move(transport);
            group->chunks = std::make_unique<BoundedQueue<ReceivedChunk>>("received-" + group->name, bufferCount,
                                                                          0, stop_, warnAfter, abortAfter);
            groups_.push_back(std::move(group));
        }
    }
    else {
        // Other transports: single client group, receive through the transport
        auto group = std::make_unique<ClientGroup>();
        group->name = transportName;
        group->transport = transport::TransportFactory::instance().build(transportName, compConf);
        groups_.push_back(std::move(group));
    }

    messages_ = std::make_unique<BoundedQueue<Message>>("messages", messageCount, messageBytes, stop_, warnAfter,
                                                        abortAfter);
    // The mailbox keeps working when the server stops: the coordinator collects all the terminations
    mailbox_ = std::make_unique<BoundedQueue<RunnableEvent>>("coordinator-mailbox", 1024, 0, neverStop_);
    activeReceivers_.store(groups_.size());

    eckit::Log::info() << "multio server: " << groups_.size() << " client group(s), transport " << transportName
                       << ", " << requiredThreads() << " runnables" << std::endl;
}

Shared::~Shared() = default;

std::size_t Shared::requiredThreads() const {
    return 3 + groups_.size();
}

std::vector<std::unique_ptr<Runnable>> Shared::createRunnables(std::size_t availableThreads) {
    if (runnablesCreated_) {
        throw eckit::SeriousBug("multio server: runnables already created", Here());
    }
    const std::size_t required = requiredThreads();
    if (availableThreads < required) {
        std::ostringstream oss;
        oss << "multio server needs " << required << " threads (listener, " << groups_.size()
            << " receiver(s), dispatcher, coordinator), " << availableThreads << " available";
        throw eckit::UserError(oss.str(), Here());
    }
    runnablesCreated_ = true;

    // The listener (MPI receives) first: it runs on the first thread of the caller
    std::vector<std::unique_ptr<Runnable>> runnables;
    runnables.push_back(std::make_unique<ListenerRunnable>());
    for (std::size_t i = 0; i < groups_.size(); ++i) {
        runnables.push_back(std::make_unique<ReceiverRunnable>(i, groups_[i]->name));
    }
    runnables.push_back(std::make_unique<DispatcherRunnable>());
    runnables.push_back(std::make_unique<CoordinatorRunnable>(required - 1));
    return runnables;
}

std::vector<QueueUsage> Shared::usage() const {
    std::vector<QueueUsage> result;
    if (receiveBuffers_) {
        result.push_back(receiveBuffers_->usage());
    }
    for (const auto& group : groups_) {
        if (group->chunks) {
            result.push_back(group->chunks->usage());
        }
    }
    result.push_back(messages_->usage());
    return result;
}

void Shared::printUsage(std::ostream& os) const {
    for (const auto& u : usage()) {
        os << "  " << u << std::endl;
    }
}

void Shared::stop() {
    stop_.store(true);
    if (receiveBuffers_) {
        receiveBuffers_->wakeAll();
    }
    for (auto& group : groups_) {
        if (group->chunks) {
            group->chunks->wakeAll();
        }
    }
    messages_->wakeAll();
}

void Shared::abortTransports(std::exception_ptr eptr) {
    for (auto& group : groups_) {
        group->transport->abort(eptr);
    }
}

transport::Transport& Shared::transportOf(const message::Peer& peer) {
    if (groups_.size() == 1) {
        return *groups_.front()->transport;
    }
    for (auto& group : groups_) {
        if (group->transport->localPeer().group() == peer.group()) {
            return *group->transport;
        }
    }
    std::ostringstream oss;
    oss << "multio server: no client group for peer " << peer;
    throw eckit::SeriousBug(oss.str(), Here());
}

void Shared::receiverFinished() {
    // The last receiver closes the message queue: the dispatcher drains it and finishes
    if (activeReceivers_.fetch_sub(1) == 1) {
        messages_->close();
    }
}

}  // namespace multio::server
