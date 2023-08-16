#pragma once

#include "multio/multio_config.h"

#include <iostream>


// #define HAVE_EXECUTION_POLICY
#ifdef HAVE_EXECUTION_POLICY
#ifdef HAVE_GNU_COMPILER
#warning "Execution policies from gnu c++17 enabled "
#include <execution>
#include <algorithm>
#endif
#ifdef HAVE_INTEL_COMPILER
#warning "Execution policies from intel c++17 enabled "
#include <oneapi/dpl/algorithm>
#include <oneapi/dpl/execution>
#endif
#ifdef HAVE_CLANG_COMPILER
#error "Execution policies from clang c++17 not tested"
#endif
#else
#include <algorithm>
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
#ifdef HAVE_GNU_COMPILER
            return std::forward<Func>(f)(std::execution::unsequenced_policy{});
#endif
#ifdef HAVE_INTEL_COMPILER
            return std::forward<Func>(f)(oneapi::dpl::execution::unsequenced_policy{});
#endif
        case (ExecutionPolicy::par_unseq):
#ifdef HAVE_GNU_COMPILER
            return std::forward<Func>(f)(std::execution::parallel_unsequenced_policy{});
#endif
#ifdef HAVE_INTEL_COMPILER
            return std::forward<Func>(f)(oneapi::dpl::execution::parallel_unsequenced_policy{});
#endif
        case (ExecutionPolicy::par):
#ifdef HAVE_GNU_COMPILER
            return std::forward<Func>(f)(std::execution::parallel_policy{});
#endif
#ifdef HAVE_INTEL_COMPILER
            return std::forward<Func>(f)(oneapi::dpl::execution::parallel_policy{});
#endif
        case (ExecutionPolicy::seq):
        default:
#ifdef HAVE_GNU_COMPILER
            return std::forward<Func>(f)(std::execution::sequenced_policy{});
#endif
#ifdef HAVE_INTEL_COMPILER
            return std::forward<Func>(f)(oneapi::dpl::execution::sequenced_policy{});
#endif
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
