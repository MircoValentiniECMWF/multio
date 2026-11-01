/*
 * (C) Copyright 2026- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @file ServerRuntime.h
/// Server runtime made of runnables that do not create threads.
///
/// The server is split in a state shared by all its parts (Shared) and in runnables (listener, one receiver
/// per client group, dispatcher, coordinator). The caller creates one thread per runnable (e.g. an OpenMP
/// region) and calls init(shared), run(drHook) and free() on each runnable from its thread.
///
/// Memory: all the data in flight is held by Shared, in bounded queues and in a receive buffer pool with
/// usage statistics. When the dispatcher is slow, the message queue fills up (its size is bounded in bytes),
/// the receivers stop releasing receive buffers, the listener stops receiving and the clients wait: the
/// memory used by the server is bounded and can be queried (Shared::usage).

#pragma once

#include <algorithm>
#include <atomic>
#include <chrono>
#include <condition_variable>
#include <cstdint>
#include <deque>
#include <iosfwd>
#include <memory>
#include <mutex>
#include <sstream>
#include <string>
#include <vector>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/io/Buffer.h"
#include "eckit/log/Log.h"

#include "multio/config/MultioConfiguration.h"
#include "multio/message/Message.h"
#include "multio/util/DrHook.h"

namespace multio::transport {
class Transport;
class MpiXTransport;
}  // namespace multio::transport

namespace multio::server {

//----------------------------------------------------------------------------------------------------------------------

/// Usage of a queue or of the receive buffer pool
struct QueueUsage {
    std::string name;
    std::uint64_t count = 0;          ///< elements currently held
    std::uint64_t bytes = 0;          ///< bytes currently held
    std::uint64_t maxCount = 0;       ///< high-water mark (elements)
    std::uint64_t maxBytes = 0;       ///< high-water mark (bytes)
    std::uint64_t capacityCount = 0;  ///< limit (elements)
    std::uint64_t capacityBytes = 0;  ///< limit (bytes, 0: no limit)
    std::uint64_t total = 0;          ///< elements pushed (buffers acquired) since the start
    double producerWaitSeconds = 0;   ///< time producers were blocked (full)
    double consumerWaitSeconds = 0;   ///< time consumers were blocked (empty)
};

std::ostream& operator<<(std::ostream& os, const QueueUsage& usage);

//----------------------------------------------------------------------------------------------------------------------

/// FIFO queue bounded in number of elements and in bytes, with usage statistics.
///
/// Producers block while the queue is full, consumers while it is empty. Blocking calls return false when
/// the queue is closed (consumers: once it is drained) or when the server stops. A producer blocked longer
/// than warnAfterSeconds logs a warning with the usage; longer than abortAfterSeconds (if > 0) it throws.
template <typename T>
class BoundedQueue {
public:
    enum class PopResult
    {
        Element,
        Timeout,
        Closed
    };

    BoundedQueue(std::string name, std::size_t maxCount, std::size_t maxBytes, const std::atomic<bool>& stop,
                 double warnAfterSeconds = 0, double abortAfterSeconds = 0) :
        name_{std::move(name)},
        maxCount_{maxCount},
        maxBytes_{maxBytes},
        stop_{stop},
        warnAfter_{warnAfterSeconds},
        abortAfter_{abortAfterSeconds} {
        ASSERT(maxCount_ > 0);
    }

    bool push(T&& elem, std::size_t bytes = 0) {
        std::unique_lock<std::mutex> lock{mutex_};
        if (full(bytes) && !closed_ && !stop_.load()) {
            const auto start = std::chrono::steady_clock::now();
            bool warned = false;
            while (full(bytes) && !closed_ && !stop_.load()) {
                notFull_.wait_for(lock, std::chrono::seconds(1));
                const double waited = secondsSince(start);
                if (!warned && warnAfter_ > 0 && waited > warnAfter_) {
                    warned = true;
                    eckit::Log::warning() << "multio server: producer blocked for " << waited << " s on full queue "
                                          << usageNoLock() << std::endl;
                }
                if (abortAfter_ > 0 && waited > abortAfter_) {
                    std::ostringstream oss;
                    oss << "multio server: producer blocked for " << waited << " s on full queue " << usageNoLock();
                    throw eckit::SeriousBug(oss.str(), Here());
                }
            }
            producerWait_ += secondsSince(start);
        }
        if (closed_ || stop_.load()) {
            return false;
        }
        items_.push_back(Item{std::move(elem), bytes});
        bytes_ += bytes;
        ++total_;
        maxCountSeen_ = std::max(maxCountSeen_, items_.size());
        maxBytesSeen_ = std::max(maxBytesSeen_, bytes_);
        notEmpty_.notify_one();
        return true;
    }

    bool pop(T& elem) { return popFor(elem, -1) == PopResult::Element; }

    /// Wait at most the given time (seconds, < 0: no limit)
    PopResult popFor(T& elem, double seconds) {
        std::unique_lock<std::mutex> lock{mutex_};
        const auto start = std::chrono::steady_clock::now();
        while (items_.empty() && !closed_ && !stop_.load()) {
            if (seconds >= 0 && secondsSince(start) >= seconds) {
                consumerWait_ += secondsSince(start);
                return PopResult::Timeout;
            }
            notEmpty_.wait_for(lock, std::chrono::seconds(1));
        }
        consumerWait_ += secondsSince(start);
        if (stop_.load() || items_.empty()) {
            return PopResult::Closed;
        }
        elem = std::move(items_.front().elem);
        bytes_ -= items_.front().bytes;
        items_.pop_front();
        notFull_.notify_one();
        return PopResult::Element;
    }

    void close() {
        std::lock_guard<std::mutex> lock{mutex_};
        closed_ = true;
        notFull_.notify_all();
        notEmpty_.notify_all();
    }

    /// Wake up the blocked producers and consumers (e.g. the server stops)
    void wakeAll() {
        std::lock_guard<std::mutex> lock{mutex_};
        notFull_.notify_all();
        notEmpty_.notify_all();
    }

    QueueUsage usage() const {
        std::lock_guard<std::mutex> lock{mutex_};
        return usageNoLock();
    }

private:
    struct Item {
        T elem;
        std::size_t bytes;
    };

    // Always accept one element, even if larger than the byte limit
    bool full(std::size_t bytes) const {
        return items_.size() >= maxCount_ || (maxBytes_ > 0 && !items_.empty() && bytes_ + bytes > maxBytes_);
    }

    static double secondsSince(std::chrono::steady_clock::time_point start) {
        return std::chrono::duration<double>(std::chrono::steady_clock::now() - start).count();
    }

    QueueUsage usageNoLock() const {
        QueueUsage u;
        u.name = name_;
        u.count = items_.size();
        u.bytes = bytes_;
        u.maxCount = maxCountSeen_;
        u.maxBytes = maxBytesSeen_;
        u.capacityCount = maxCount_;
        u.capacityBytes = maxBytes_;
        u.total = total_;
        u.producerWaitSeconds = producerWait_;
        u.consumerWaitSeconds = consumerWait_;
        return u;
    }

    const std::string name_;
    const std::size_t maxCount_;
    const std::size_t maxBytes_;
    const std::atomic<bool>& stop_;
    const double warnAfter_;
    const double abortAfter_;

    mutable std::mutex mutex_;
    std::condition_variable notFull_;
    std::condition_variable notEmpty_;
    std::deque<Item> items_;
    bool closed_ = false;
    std::size_t bytes_ = 0;
    std::size_t maxCountSeen_ = 0;
    std::size_t maxBytesSeen_ = 0;
    std::uint64_t total_ = 0;
    double producerWait_ = 0;
    double consumerWait_ = 0;
};

//----------------------------------------------------------------------------------------------------------------------

/// Fixed set of receive buffers (the only place where the server holds received MPI data)
class ReceiveBufferPool {
public:
    ReceiveBufferPool(std::size_t count, std::size_t bufferSize, const std::atomic<bool>& stop,
                      double warnAfterSeconds);

    /// Blocks until a buffer is free; returns -1 if the server stops
    long acquire();
    void release(long index);
    eckit::Buffer& buffer(long index);

    void wakeAll();
    QueueUsage usage() const;

private:
    const std::size_t bufferSize_;
    const std::atomic<bool>& stop_;
    const double warnAfter_;
    std::vector<std::unique_ptr<eckit::Buffer>> buffers_;

    mutable std::mutex mutex_;
    std::condition_variable released_;
    std::vector<long> free_;
    std::size_t maxInUse_ = 0;
    std::uint64_t acquired_ = 0;
    double waited_ = 0;
};

/// Received MPI message, in a buffer of the pool (listener -> receiver)
struct ReceivedChunk {
    long buffer = -1;
    std::size_t size = 0;
};

/// Runnable termination notified to the coordinator
struct RunnableEvent {
    std::string runnable;
    bool error = false;
    std::string what;
};

//----------------------------------------------------------------------------------------------------------------------

class Runnable;

/// State shared by the runnables of a server: configuration, transports (one per client group), queues,
/// receive buffers and control (stop flag, coordinator mailbox).
class Shared : private config::MultioConfigurationHolder {
public:
    /// A group of clients, connected to the server through an extended communicator
    struct ClientGroup {
        std::string name;
        std::unique_ptr<transport::Transport> transport;
        transport::MpiXTransport* mpix = nullptr;  ///< set: receive without transport internal buffers
        std::unique_ptr<BoundedQueue<ReceivedChunk>> chunks;  ///< listener -> receiver (mpix only)
        std::atomic<bool> finished{false};
    };

    explicit Shared(config::MultioConfiguration&& multioConf);
    ~Shared();

    Shared(const Shared&) = delete;
    Shared& operator=(const Shared&) = delete;

    /// Number of threads (runnables) the server needs: listener, one receiver per group, dispatcher, coordinator
    std::size_t requiredThreads() const;

    /// Create the runnables (at most availableThreads; throws if not enough). Can be called once.
    std::vector<std::unique_ptr<Runnable>> createRunnables(std::size_t availableThreads);

    /// Memory usage of the queues and of the receive buffers
    std::vector<QueueUsage> usage() const;
    void printUsage(std::ostream& os) const;

    /// Stop the server: blocked queue operations return and the runnables finish as soon as possible
    void stop();
    bool stopping() const { return stop_.load(); }

    /// Abort the transports (e.g. MPI_Abort)
    void abortTransports(std::exception_ptr eptr);

    // Used by the runnables
    config::MultioConfiguration& configuration() { return multioConfig(); }
    const eckit::LocalConfiguration& serverConfig() const { return serverConf_; }
    std::vector<std::unique_ptr<ClientGroup>>& groups() { return groups_; }
    ReceiveBufferPool* receiveBuffers() { return receiveBuffers_.get(); }
    BoundedQueue<message::Message>& messages() { return *messages_; }
    BoundedQueue<RunnableEvent>& mailbox() { return *mailbox_; }
    transport::Transport& transportOf(const message::Peer& peer);
    void receiverFinished();
    double usageLogSeconds() const { return usageLogSeconds_; }

private:
    eckit::LocalConfiguration serverConf_;
    std::atomic<bool> stop_{false};
    std::atomic<bool> neverStop_{false};
    std::atomic<std::size_t> activeReceivers_{0};
    bool runnablesCreated_ = false;
    double usageLogSeconds_ = 0;

    std::vector<std::unique_ptr<ClientGroup>> groups_;
    std::unique_ptr<ReceiveBufferPool> receiveBuffers_;
    std::unique_ptr<BoundedQueue<message::Message>> messages_;
    std::unique_ptr<BoundedQueue<RunnableEvent>> mailbox_;
};

//----------------------------------------------------------------------------------------------------------------------

/// Part of the server executed by one thread of the caller: init(shared), run(drHook) and free() are called,
/// in this order, by the thread that executes the runnable.
///
/// The work is done by step() (one unit of work, returns false when the runnable has finished); run() loops
/// over step(). Errors never leave run(): they are reported to the coordinator and run() returns non-zero.
class Runnable {
public:
    explicit Runnable(std::string name) : name_{std::move(name)} {}
    virtual ~Runnable() = default;

    virtual std::string kind() const = 0;
    const std::string& name() const { return name_; }

    void init(Shared& shared);
    int run(const util::DrHook& drHook);
    void free();

    /// Error message of a failed run
    const std::string& error() const { return error_; }

protected:
    virtual void setup() {}
    virtual bool step() = 0;
    virtual void teardown() {}

    /// Runnables that do not notify their termination to the coordinator (the coordinator itself)
    virtual bool notifiesCoordinator() const { return true; }

    Shared& shared();
    const util::DrHook& drHook() const { return drHook_; }

private:
    enum class State
    {
        Created,
        Initialised,
        Finished,
        Freed
    };

    std::string name_;
    Shared* shared_ = nullptr;
    util::DrHook drHook_;
    std::string error_;
    State state_ = State::Created;
};

}  // namespace multio::server
