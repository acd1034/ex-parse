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
#include <optional>
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
  };
  struct Punct {
    std::string_view data;
  };
  struct Eof {};
  struct Invalid {
    std::string_view data;
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

  // ----- parse -----

// https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2023/p2561r2.html
#define NS_EXPECTED_TRY_IMPL(var, result)                                      \
  auto&& ns_temporary##var = (result);                                         \
  if (not ns_temporary##var)                                                   \
    return std::unexpected{                                                    \
        std::forward<decltype(ns_temporary##var)>(ns_temporary##var).error()}; \
  [[maybe_unused]] auto&& var =                                                \
      *std::forward<decltype(ns_temporary##var)>(ns_temporary##var)

#define NS_EXPECTED_TRY(var, result) NS_EXPECTED_TRY_IMPL(var, result)

  struct Error {
    std::string msg;
  };

  struct FunCall;
  struct ASTIdent;
  using Expr = std::variant<FunCall, ASTIdent>;
  struct FunCall {
    std::string name{};
    std::vector<Expr> args{};
  };
  struct ASTIdent {
    std::string name{};
  };

  std::optional<Ident> consume_ident(Tokenizer& it) {
    Token tok = *it;
    if (auto ident = std::get_if<Ident>(&tok)) {
      ++it;
      return std::move(*ident);
    } else {
      return std::nullopt;
    }
  }

  bool consume_punct(Tokenizer& it, std::string_view punct) {
    Token tok = *it;
    if (auto p = std::get_if<Punct>(&tok); p and p->data == punct) {
      ++it;
      return true;
    } else {
      return false;
    }
  }

  bool consume_eof(Tokenizer& it) {
    Token tok = *it;
    if (std::get_if<Eof>(&tok)) {
      ++it;
      return true;
    } else {
      return false;
    }
  }

  std::expected<Ident, Error> expect_ident(Tokenizer& it) {
    Token tok = *it;
    if (auto ident = std::get_if<Ident>(&tok)) {
      ++it;
      return std::move(*ident);
    } else {
      return std::unexpected{Error{"unexpected token, expecting identifier"}};
    }
  }

  std::expected<Punct, Error> //
  expect_punct(Tokenizer& it, std::string_view punct) {
    Token tok = *it;
    if (auto p = std::get_if<Punct>(&tok); p and p->data == punct) {
      ++it;
      return std::move(*p);
    } else {
      return std::unexpected{Error{"unexpected token, expecting punctuator"}};
    }
  }

  std::expected<Eof, Error> expect_eof(Tokenizer& it) {
    Token tok = *it;
    if (auto e = std::get_if<Eof>(&tok)) {
      ++it;
      return std::move(*e);
    } else {
      return std::unexpected{Error{"unexpected token, expecting EOF"}};
    }
  }

  /*
   * expr     = primary
   * primary  = ident ("(" fun_args)?
   * fun_args = (expr ("," expr)*)? ")"
   */

  std::expected<Expr, Error> parse_expr(Tokenizer& it);

  // fun_args = (expr ("," expr)*)? ")"
  std::expected<std::vector<Expr>, Error> parse_fun_args(Tokenizer& it) {
    std::vector<Expr> args{};

    if (consume_punct(it, ")")) {
      return args;
    }

    NS_EXPECTED_TRY(expr, parse_expr(it));
    args.push_back(std::move(expr));
    while (consume_punct(it, ",")) {
      NS_EXPECTED_TRY(expr2, parse_expr(it));
      args.push_back(std::move(expr2));
    }

    NS_EXPECTED_TRY(_p, expect_punct(it, ")"));
    return args;
  }

  // primary  = ident ("(" fun_args)?
  std::expected<Expr, Error> parse_primary(Tokenizer& it) {
    NS_EXPECTED_TRY(name, expect_ident(it));

    if (consume_punct(it, "(")) {
      NS_EXPECTED_TRY(args, parse_fun_args(it));
      return Expr(FunCall{std::move(name.data), std::move(args)});
    }

    return Expr(ASTIdent{std::move(name.data)});
  }

  // expr     = primary
  std::expected<Expr, Error> parse_expr(Tokenizer& it) {
    return parse_primary(it);
  }
} // namespace ns
