
#ifndef multio_sandbox_TestData_H
#define multio_sandbox_TestData_H

#include <random>

#include "eckit/mpi/Comm.h"

#include "sandbox/Peer.h"

namespace multio {
namespace sandbox {

// Default global values, but each test is allowed to override them
inline size_t& field_size() {
    static size_t val;
    return (!val ? (val = 23) : val);
}

inline size_t& root() {
    static size_t rt;  // = 0 if not set to another value
    return rt;
}

inline bool& new_random_data_each_run() {
    static bool val = false;
    return val;
}

inline std::vector<double> create_hashed_data(const std::string& field_id, const size_t sz) {
    std::vector<double> field(sz);

    auto ii = 0;
    generate(begin(field), end(field), [&ii, &field_id]() {
        auto hash_val = std::hash<std::string>{}(field_id + std::to_string(ii++));
        hash_val = static_cast<uint32_t>(hash_val >> 32);
        return 13.0 + 17.0 * static_cast<double>(hash_val) /
                          static_cast<double>(std::numeric_limits<uint32_t>::max());
    });

    return field;
}

inline std::vector<double> create_random_data(const size_t sz) {
    std::vector<double> field;

    std::random_device rd;  // Will be used to obtain a seed for the random number engine
    std::mt19937 gen(rd()); //Standard mersenne_twister_engine seeded with rd()
    std::uniform_real_distribution<double> dis(13.0, 30.0);

    for (auto ii = 0u; ii != sz; ++ii) {
        field.push_back(dis(gen));
    }

    return field;
}

inline std::vector<size_t> generate_index_map(size_t id, size_t nbclients) {
    auto chunk_size = field_size() / nbclients + ((id < field_size() % nbclients) ? 1 : 0);

    auto maps = std::vector<size_t>(chunk_size);
    for (auto jj = 0u; jj != chunk_size; ++jj) {
        maps[jj] = static_cast<size_t>(id) + jj * nbclients;
    }
    return maps;
}

inline std::vector<double>& global_test_field(const std::string& field_id, const size_t sz,
                                              const std::string& transport, const size_t list_id) {
    using eckit::mpi::comm;

    static std::map<std::string, std::vector<double>> test_fields;
    if (test_fields.find(field_id) != end(test_fields)) {
        return test_fields[field_id];
    }

    if (transport == "Mpi" && new_random_data_each_run()) {
        test_fields[field_id] =
            (root() == list_id) ? create_random_data(sz) : std::vector<double>(sz);
        comm().broadcast(test_fields[field_id], root());

        return test_fields[field_id];
    }

    test_fields[field_id] =
        new_random_data_each_run() ? create_random_data(sz) : create_hashed_data(field_id, sz);

    return test_fields[field_id];
}

}  // namespace sandbox
}  // namespace multio

#endif
