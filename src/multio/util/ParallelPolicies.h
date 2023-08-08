#pragma once

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
        case (Policy::unseq):
            return std::forward<Func>(f)(std::execution::unsequenced_policy{});
        case (Policy::par_unseq):
            return std::forward<Func>(f)(std::execution::parallel_unsequenced_policy{});
        case (Policy::par):
            return std::forward<Func>(f)(std::execution::parallel_policy{});
        case (Policy::seq):
        // Fallthrough <3 Razvan
        default:
            return std::forward<Func>(f)(std::execution::sequenced_policy{});
    }
};
#endif

template <typename... Args>
void transform(ExecutionPolicy t, Args&&... args) {
#ifdef __USE_EXECUTION_POLICIES__
    dispatchExecutionPolicy(t, [&](auto exPol) { std::transform(exPol, std::forward<Args>(args)...); });

#else
    std::transform(std::forward<Args>(args)...);
#endif
};

}  // namespace multio::util
