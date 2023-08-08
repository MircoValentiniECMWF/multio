#pragma once

#include <type_traits>

namespace multio::util {

template <typename OutType, typename InType, std::enable_if_t<std::is_same<InType, OutType>::value, bool> = true>
inline OutType staticCastValueMaybe(InType v) noexcept {
    return v;
}

template <typename OutType, typename InType, std::enable_if_t<!std::is_same<InType, OutType>::value, bool> = true>
inline OutType staticCastValueMaybe(InType v) noexcept {
    return static_cast<OutType>(v);
}


}  // namespace multio::util