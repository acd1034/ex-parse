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

  template <std::size_t I, class T>
  struct indexed {};

  template <class Seq, class... Ts>
  struct indexer;

  template <std::size_t... Is, class... Ts>
  struct indexer<std::index_sequence<Is...>, Ts...> : indexed<Is, Ts>... {};

  template <class T, std::size_t I>
  std::integral_constant<std::size_t, I> get_index(
      const indexed<I, T>&); // undefined

  /// variant_alternative_index
  template <class T, class Variant>
  struct variant_alternative_index;

  template <class T, class Variant>
  struct variant_alternative_index<T, const Variant>
      : variant_alternative_index<T, Variant> {};

  template <class T, class... Ts>
  struct variant_alternative_index<T, std::variant<Ts...>>
      : decltype(get_index<T>(
            indexer<std::index_sequence_for<Ts...>, Ts...>{})) {};

  template <class T, class Variant>
  inline constexpr std::size_t variant_alternative_index_v =
      variant_alternative_index<T, Variant>::value;

  /// overloaded
  template <class... Ts>
  struct overloaded : Ts... {
    using Ts::operator()...;
  };
  template <class... Ts>
  overloaded(Ts...) -> overloaded<Ts...>;

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
} // namespace ns

template <>
struct std::formatter<ns::Token> {
  constexpr auto parse(std::format_parse_context& ctx)
      -> decltype(ctx.begin()) {
    auto it = ctx.begin();
    if (it != ctx.end() and *it != '}') {
      throw std::format_error{"invalid format"};
    }
    return it;
  }

  auto format(const ns::Token& token, auto& ctx) const -> decltype(ctx.out()) {
    auto out = ctx.out();

    /* NOTE: variant visit:
     * 1. if get_if
     * 2. switch index + variant_alternative_index_v
     * https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2023/p2527r2.html#word
     * 3. overloaded
     * https://en.cppreference.com/w/cpp/utility/variant/visit
     */

    // clang-format off
    if (auto ident = std::get_if<ns::Ident>(&token))
      out = std::format_to(out, "Ident(\"{}\")", ident->data);
    else if (auto punct = std::get_if<ns::Punct>(&token))
      out = std::format_to(out, "Punct(\"{}\")", punct->data);
    else if (std::get_if<ns::Eof>(&token))
      out = std::format_to(out, "Eof");
    else if (auto invalid = std::get_if<ns::Invalid>(&token))
      out = std::format_to(out, "Invalid(\"{}\")", invalid->data);
    else
      std::unreachable();

    /* switch (token.index()) {
      case 0:
        out = std::format_to(out, "Ident(\"{}\")", std::get<0>(token).data);
        break;
      case 1:
        out = std::format_to(out, "Punct(\"{}\")", std::get<1>(token).data);
        break;
      case 2:
        out = std::format_to(out, "Eof");
        break;
      case 3:
        out = std::format_to(out, "Invalid(\"{}\")", std::get<3>(token).data);
        break;
      default:
      std::unreachable();
    } */

    /* switch (token.index()) {
      case ns::variant_alternative_index_v<ns::Ident, ns::Token>:
        out = std::format_to(out, "Ident(\"{}\")", std::get<ns::Ident>(token).data);
        break;
      case ns::variant_alternative_index_v<ns::Punct, ns::Token>:
        out = std::format_to(out, "Punct(\"{}\")", std::get<ns::Punct>(token).data);
        break;
      case ns::variant_alternative_index_v<ns::Eof, ns::Token>:
        out = std::format_to(out, "Eof");
        break;
      case ns::variant_alternative_index_v<ns::Invalid, ns::Token>:
        out = std::format_to(out, "Invalid(\"{}\")", std::get<ns::Invalid>(token).data);
        break;
      default:
        std::unreachable();
    } */

    /* struct fn {
      decltype(ctx.out()) out;
      auto operator()(const ns::Ident& ident) { return std::format_to(out, "Ident(\"{}\")", ident.data); }
      auto operator()(const ns::Punct& punct) { return std::format_to(out, "Punct(\"{}\")", punct.data); }
      auto operator()(const ns::Eof&) { return std::format_to(out, "Eof"); }
      auto operator()(const ns::Invalid& invalid) { return std::format_to(out, "Invalid(\"{}\")", invalid.data); }
    };
    out = std::visit(fn{out}, token); */
    /* auto fn = [out](const auto& tok) {
      if constexpr (ns::similar_to<decltype(tok), ns::Ident>)
        return std::format_to(out, "Ident(\"{}\")", tok.data);
      else if constexpr (ns::similar_to<decltype(tok), ns::Punct>)
        return std::format_to(out, "Punct(\"{}\")", tok.data);
      else if constexpr (ns::similar_to<decltype(tok), ns::Eof>)
        return std::format_to(out, "Eof");
      else if constexpr (ns::similar_to<decltype(tok), ns::Invalid>)
        return std::format_to(out, "Invalid(\"{}\")", tok.data);
      else
        std::unreachable();
    };
    out = std::visit(fn, token); */
    auto fn = ns::overloaded{
      [out](const ns::Ident& ident) { return std::format_to(out, "Ident(\"{}\")", ident.data); },
      [out](const ns::Punct& punct) { return std::format_to(out, "Punct(\"{}\")", punct.data); },
      [out](const ns::Eof&) { return std::format_to(out, "Eof"); },
      [out](const ns::Invalid& invalid) { return std::format_to(out, "Invalid(\"{}\")", invalid.data); }
    };
    out = std::visit(fn, token);
    /* out = token.visit(ns::overloaded{
      [out](const ns::Ident& ident) { return std::format_to(out, "Ident(\"{}\")", ident.data); },
      [out](const ns::Punct& punct) { return std::format_to(out, "Punct(\"{}\")", punct.data); },
      [out](const ns::Eof&) { return std::format_to(out, "Eof"); },
      [out](const ns::Invalid& invalid) { return std::format_to(out, "Invalid(\"{}\")", invalid.data); }
    }); */
    // clang-format on

    return out;
  }
};

namespace ns {
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

  class Tokenizer {
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

  // NOTE: std::iterator_interface
  /* class Tokenizer
      : std::iterator_interface<std::forward_iterator_tag, Token, Token> {
    Token token_{};
    std::string_view input_{};

  public:
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
    friend bool operator==(const Tokenizer&, const Tokenizer&) = default;
  }; */

  // ----- parse -----

// NOTE: Try Macro | try?
// BOOST_LEAF_ASSIGN:
// https://www.boost.org/doc/libs/1_83_0/libs/leaf/doc/html/index.html#BOOST_LEAF_ASSIGN
// BOOST_LEAF_AUTO(var, result); は以下のように展開される
// auto&& <<temp>> = result;
// if (not <<temp>>)
//   return <<temp>>.error();
// auto var = *std::forward<decltype(<<temp>>)>(<<temp>>);
// BOOST_OUTCOME_TRY:
// https://boostorg.github.io/outcome/tutorial/essential/result/try.html
// try?:
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

  struct Variable;
  struct FunCall;
  using Expr = std::variant<Variable, FunCall>;
  struct Variable {
    std::string name{};
  };
  struct FunCall {
    std::string name{};
    std::vector<Expr> args{};
  };

  std::optional<Ident> consume_ident(Tokenizer& it) {
    Token tok = *it;

    // NOTE: if let → std::get_if

    /* if (std::holds_alternative<Ident>(tok)) {
      ++it;
      return std::move(std::get<Ident>(tok));
    } else {
      return std::nullopt;
    } */

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
      return std::unexpected{Error{
          std::format("unexpected token `{}`, expecting identifier", tok)}};
    }
  }

  std::expected<Punct, Error> //
  expect_punct(Tokenizer& it, std::string_view punct) {
    Token tok = *it;
    if (auto p = std::get_if<Punct>(&tok); p and p->data == punct) {
      ++it;
      return std::move(*p);
    } else {
      return std::unexpected{Error{std::format(
          "unexpected token `{}`, expecting punctuator `\"{}\"`", tok, punct)}};
    }
  }

  std::expected<Eof, Error> expect_eof(Tokenizer& it) {
    Token tok = *it;
    if (auto e = std::get_if<Eof>(&tok)) {
      ++it;
      return std::move(*e);
    } else {
      return std::unexpected{
          Error{std::format("unexpected token `{}`, expecting EOF", tok)}};
    }
  }

  //' program  = expr EOF
  //' expr     = IDENT
  //'          | IDENT "(" fun_args
  //' fun_args = ")"
  //'          | expr ("," expr)* ")"

  std::expected<Expr, Error> parse_expr(Tokenizer& it);

  //' fun_args = ")"
  //'          | expr ("," expr)* ")"
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

  //' expr     = ident
  //'          | ident "(" fun_args
  std::expected<Expr, Error> parse_expr(Tokenizer& it) {
    NS_EXPECTED_TRY(name, expect_ident(it));

    if (consume_punct(it, "(")) {
      NS_EXPECTED_TRY(args, parse_fun_args(it));
      return Expr(FunCall{std::move(name.data), std::move(args)});
    }

    return Expr(Variable{std::move(name.data)});
  }

  //' program  = expr EOF
  std::expected<Expr, Error> parse_program(Tokenizer& it) {
    NS_EXPECTED_TRY(expr, parse_expr(it));
    NS_EXPECTED_TRY(_eof, expect_eof(it));
    return expr;
  }

  // NOTE: std::expectedの構築・処理方法
  // 1. std::expectedの構築
  std::expected<Ident, Error> parse_ident(Tokenizer& it) {
    Token tok = *it;
    if (auto ident = std::get_if<Ident>(&tok)) {
      ++it;
      return *ident;
    } else {
      return std::unexpected{Error{
          std::format("unexpected token `{}`, expecting identifier", tok)}};
    }
  }
  // 2. std::expectedの処理
  // variable = IDENT
  std::expected<Expr, Error> parse_variable(Tokenizer& it) {
    auto name = parse_ident(it);
    if (name)
      return Expr(Variable{name->data});
    else
      return std::unexpected{name.error()};
  }
  // fun_call = IDENT "(" fun_args
  /* std::expected<Expr, Error> parse_fun_call(Tokenizer& it) {
    auto name = parse_ident(it);
    if (not name)
      return std::unexpected{name.error()};

    auto _p = parse_punct(it, "(");
    if (not _p)
      return std::unexpected{_p.error()};

    auto args = parse_fun_args(it);
    if (not args)
      return std::unexpected{args.error()};

    return Expr(FunCall{name->data, *args});
  } */
  /* std::expected<Expr, Error> parse_fun_call(Tokenizer& it) {
    NS_EXPECTED_TRY(name, parse_ident(it));
    NS_EXPECTED_TRY(_p, parse_punct(it, "("));
    NS_EXPECTED_TRY(args, parse_fun_args(it));
    return Expr(FunCall{name.data, args});
  } */
  /* std::expected<Expr, Error> parse_fun_call(Tokenizer& it) {
    Ident name = parse_ident(it).try?;
    parse_punct(it, "(").try?;
    std::vector<Expr> args = parse_fun_args(it).try?;
    return Expr(FunCall{name.data, args});
  } */

  // ----- count_fun_call -----

  // NOTE: 再帰的なvisitor
  // 1. struct
  // clang-format off
  int count_fun_call(const Expr& expr) {
    struct fn {
      int operator()(const FunCall& funcall) const {
        int count = 1;
        for (const auto& arg : funcall.args)
          count += std::visit(*this, arg);
        return count;
      }
      int operator()(const Variable&) const { return 0; }
    };
    return std::visit(fn{}, expr);
  }
  // 2. lambda + if constexpr
  /* int count_fun_call(const Expr& expr) {
    constexpr auto fn = [](this const auto& self, const auto& expr) {
      if constexpr (similar_to<decltype(expr), FunCall>) {
        int count = 1;
        for (const auto& arg : funcall.args)
          count += std::visit(self, arg);
        return count;
      } else if constexpr (similar_to<decltype(expr), Variable>)
        return 0;
      else
        std::unreachable();
    };
    return std::visit(fn, expr);
  } */
} // namespace ns
