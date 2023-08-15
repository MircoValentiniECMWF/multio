#pragma once

#include <iostream>
#include <algorithm>
#include "multio/multio_config.h"


// #define HAVE_EXECUTION_POLICY
#ifdef HAVE_EXECUTION_POLICY
#warning "Execution policies from c++17 enabled "
#include <execution>
#endif

namespace multio::util {

enum class ExecutionPolicy
{
    seq,
    unseq,
    par_unseq,
    par
};

#ifdef HAVE_EXECUTION_POLICY
template <typename Func>
decltype(auto) dispatchExecutionPolicy(ExecutionPolicy t, Func&& f) {
    switch (t) {
        case (ExecutionPolicy::unseq):
            return std::forward<Func>(f)(std::execution::unsequenced_policy{});
        case (ExecutionPolicy::par_unseq):
            return std::forward<Func>(f)(std::execution::parallel_unsequenced_policy{});
        case (ExecutionPolicy::par):
            return std::forward<Func>(f)(std::execution::parallel_policy{});
        case (ExecutionPolicy::seq):
        default:
            return std::forward<Func>(f)(std::execution::sequenced_policy{});
    }
};
#endif


template <typename... Args>
void transform(ExecutionPolicy t, Args&&... args) {
#ifdef HAVE_EXECUTION_POLICY
    dispatchExecutionPolicy(t, [&](auto exPol) { std::transform(exPol, std::forward<Args>(args)...); } );
#else
    std::transform(std::forward<Args>(args)...);
#endif
    return;
};

}  // namespace multio::util
