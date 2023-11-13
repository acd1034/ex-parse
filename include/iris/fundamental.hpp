/// @file fundamental.hpp
#pragma once
#include <cassert>
#include <compare>
#include <concepts>
#include <cstddef> // std::size_t, std::ptrdiff_t, std::nullptr_t
#include <cstdint> // std::int32_t
#include <initializer_list>
#include <tuple>
#include <type_traits>
#include <utility> // std::move, std::forward, std::swap, std::exchange

namespace iris {
  /// similar_to
  template <class T, class U>
  concept similar_to =
    std::same_as<std::remove_cvref_t<T>, std::remove_cvref_t<U>>;
} // namespace iris
