/// @file parse.hpp
#pragma once
#include <algorithm> // std::find_if_not
#include <cassert>
#include <cctype>
#include <compare>
#include <concepts>
#include <expected>
#include <format>
#include <iterator>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <utility>
#include <variant>
#include <vector>

namespace ns {
  /// similar_to
  template <class T, class U>
  concept similar_to =
      std::same_as<std::remove_cvref_t<T>, std::remove_cvref_t<U>>;

  // ----- Tokenizer -----

  struct Ident {
    std::string data;
    friend bool operator==(const Ident&, const Ident&) = default;
  };
  struct Punct {
    std::string_view data;
    friend bool operator==(const Punct&, const Punct&) = default;
  };
  struct Eof {
    friend bool operator==(const Eof&, const Eof&) = default;
  };
  struct Invalid {
    std::string_view data;
    friend bool operator==(const Invalid&, const Invalid&) = default;
  };
  using Token = std::variant<Ident, Punct, Eof, Invalid>;

  inline constexpr auto isspace = [](char c) -> bool {
    return std::isspace(static_cast<unsigned char>(c));
  };
  inline constexpr auto isident0 = [](char c) -> bool {
    return std::isalpha(static_cast<unsigned char>(c)) or c == '_';
  };
  inline constexpr auto isident1 = [](char c) -> bool {
    return std::isalnum(static_cast<unsigned char>(c)) or c == '_';
  };
  inline constexpr auto ispunct = [](char c) -> bool {
    return std::ispunct(static_cast<unsigned char>(c));
  };

  std::pair<Token, std::string_view> tokenize(std::string_view input) {
    if (input.empty()) {
      return {Eof{}, input};
    }

    if (isspace(input[0])) {
      auto it = std::find_if_not(input.begin(), input.end(), isspace);
      auto pos = static_cast<std::size_t>(it - input.begin());
      return tokenize(input.substr(pos));
    }

    if (isident0(input[0])) {
      auto it = std::find_if_not(input.begin(), input.end(), isident1);
      auto pos = static_cast<std::size_t>(it - input.begin());
      return {Ident{std::string(input.substr(0, pos))}, input.substr(pos)};
    }

    if (ispunct(input[0])) {
      return {Punct{input.substr(0, 1)}, input.substr(1)};
    }

    return {Invalid{input.substr(0, 1)}, input.substr(1)};
  }

  struct Tokenizer {
  private:
    Token token_{};
    std::string_view input_{};

  public:
    using difference_type = std::ptrdiff_t;
    using value_type = Token;
    using iterator_concept = std::forward_iterator_tag;

    Tokenizer() = default;

    explicit Tokenizer(std::string_view input) {
      std::tie(token_, input_) = tokenize(input);
    }

    value_type operator*() const {
      return token_;
    }

    Tokenizer& operator++() {
      std::tie(token_, input_) = tokenize(input_);
      return *this;
    }

    Tokenizer operator++(int) {
      Tokenizer tmp = *this;
      ++*this;
      return tmp;
    }

    friend bool operator==(const Tokenizer&, const Tokenizer&) = default;
  };

  static_assert(std::forward_iterator<Tokenizer>);
} // namespace ns
