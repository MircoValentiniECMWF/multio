
#include "MultioServer.h"

#include <fstream>
#include <iomanip>
#include <sstream>
#include <thread>
#include <vector>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/filesystem/PathName.h"
#include "eckit/log/Statistics.h"
#include "eckit/types/DateTime.h"

#include "multio/LibMultio.h"
#include "multio/transport/Transport.h"
#include "multio/transport/TransportRegistry.h"

namespace multio::server {



MultioServer::MultioServer(MultioConfiguration&& multioConf) :
    detail::SharedHolder(std::move(multioConf)),
    FailureAware(config::ComponentConfiguration(shared_->serverConfig(), shared_->configuration())) {
    eckit::Log::info() << "Server start listening..." << std::endl;
    withFailureHandling([&]() { run(); });
    eckit::Log::info() << "Listening loop has stopped" << std::endl;
}

MultioServer::~MultioServer() = default;

void MultioServer::run() {
    auto runnables = shared_->createRunnables(shared_->requiredThreads());

    std::vector<int> status(runnables.size(), 0);
    std::vector<std::thread> threads;
    for (std::size_t i = 0; i < runnables.size(); ++i) {
        threads.emplace_back([this, &runnables, &status, i]() {
            runnables[i]->init(*shared_);
            status[i] = runnables[i]->run(util::DrHook{});
            runnables[i]->free();
        });
    }
    for (auto& thread : threads) {
        thread.join();
    }

    std::ostringstream errors;
    for (std::size_t i = 0; i < runnables.size(); ++i) {
        if (status[i] != 0) {
            errors << "  " << runnables[i]->name() << ": " << runnables[i]->error() << std::endl;
        }
    }
    if (!errors.str().empty()) {
        throw eckit::SeriousBug("multio server failed:\n" + errors.str(), Here());
    }
}

util::FailureHandlerResponse MultioServer::handleFailure(util::OnServerError t, const util::FailureContext& c,
                                                         util::DefaultFailureState&) const {
    // Last cascading instace - print nested contexts
    eckit::Log::error() << c << std::endl;
    eckit::Log::flush();

    if (t == util::OnServerError::AbortTransport) {
        shared_->abortTransports(c.eptr);
    }
    return util::FailureHandlerResponse::Rethrow;
};

}  // namespace multio::server
