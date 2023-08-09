#pragma once

#include <iostream>
#include <algorithm>


// #define __USE_EXECUTION_POLICIES__
#ifdef __USE_EXECUTION_POLICIES__
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

#ifdef __USE_EXECUTION_POLICIES__
template <typename Func>
decltype(auto) dispatchExecutionPolicy(ExecutionPolicy t, Func&& f) {
    switch (t) {
        case (ExecutionPolicy::unseq):{
            std::cout << "POLICY :: unseq" << std::endl;
            return std::forward<Func>(f)(std::execution::unsequenced_policy{});
        }
        case (ExecutionPolicy::par_unseq):{
            std::cout << "POLICY :: par_unseq" << std::endl;
            return std::forward<Func>(f)(std::execution::parallel_unsequenced_policy{});
        }
        case (ExecutionPolicy::par):{
            std::cout << "POLICY :: par" << std::endl;
            return std::forward<Func>(f)(std::execution::parallel_policy{});
        }
        case (ExecutionPolicy::seq):
        default:{
            std::cout << "POLICY :: seq" << std::endl;
            return std::forward<Func>(f)(std::execution::sequenced_policy{});
        }
    }
};
#endif


template <typename... Args>
void transform(ExecutionPolicy t, Args&&... args) {
#ifdef __USE_EXECUTION_POLICIES__
    dispatchExecutionPolicy(t, [&](auto exPol) { std::transform(exPol, std::forward<Args>(args)...); } );
#else
    std::transform(std::forward<Args>(args)...);
#endif
    return;
};

}  // namespace multio::util
