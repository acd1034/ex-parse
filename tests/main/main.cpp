#include <catch2/catch_test_macros.hpp>
#include <iris/parse.hpp>

TEST_CASE("main", "[main][similar_to]") {
  CHECK(ns::similar_to<const int&, int>);
}

TEST_CASE("tokenize", "[tokenize]") {
  {
    std::string_view in = "\1";
    auto [token, out] = ns::tokenize(in);
    auto invalid = std::get_if<ns::Invalid>(&token);
    CHECK(invalid);
    CHECK(invalid->data.compare("\1") == 0);
    CHECK(out.compare("") == 0);
  }
  {
    std::string_view in = "";
    auto [token, out] = ns::tokenize(in);
    CHECK(std::get_if<ns::Eof>(&token));
    CHECK(out.compare("") == 0);
  }
  {
    std::string_view in = "     ";
    auto [token, out] = ns::tokenize(in);
    CHECK(std::get_if<ns::Eof>(&token));
    CHECK(out.compare("") == 0);
  }
  {
    std::string_view in = "ident_0";
    auto [token, out] = ns::tokenize(in);
    auto ident = std::get_if<ns::Ident>(&token);
    CHECK(ident);
    CHECK(ident->data.compare("ident_0") == 0);
    CHECK(out.compare("") == 0);
  }
  {
    std::string_view in = ",";
    auto [token, out] = ns::tokenize(in);
    auto punct = std::get_if<ns::Punct>(&token);
    CHECK(punct);
    CHECK(punct->data.compare(",") == 0);
    CHECK(out.compare("") == 0);
  }
  {
    std::string_view in = "()";
    auto [token, out] = ns::tokenize(in);
    auto punct = std::get_if<ns::Punct>(&token);
    CHECK(punct);
    CHECK(punct->data.compare("(") == 0);
    CHECK(out.compare(")") == 0);
  }
  {
    std::string_view in = "Neg(input)";
    ns::Tokenizer it(in);
    {
      auto token = *it++;
      auto ident = std::get_if<ns::Ident>(&token);
      CHECK(ident);
      CHECK(ident->data.compare("Neg") == 0);
    }
    {
      auto token = *it++;
      auto punct = std::get_if<ns::Punct>(&token);
      CHECK(punct);
      CHECK(punct->data.compare("(") == 0);
    }
    {
      auto token = *it++;
      auto ident = std::get_if<ns::Ident>(&token);
      CHECK(ident);
      CHECK(ident->data.compare("input") == 0);
    }
    {
      auto token = *it++;
      auto punct = std::get_if<ns::Punct>(&token);
      CHECK(punct);
      CHECK(punct->data.compare(")") == 0);
    }
    {
      auto token = *it++;
      auto eof = std::get_if<ns::Eof>(&token);
      CHECK(eof);
    }
    {
      auto token = *it++;
      auto eof = std::get_if<ns::Eof>(&token);
      CHECK(eof);
    }
  }
}

TEST_CASE("parse", "[parse]") {
  {
    std::string_view in = "Const()";
    ns::Tokenizer it(in);
    auto result = ns::parse_expr(it);
    CHECK(result);
    auto funcall = std::get_if<ns::FunCall>(&*result);
    CHECK(funcall);
    CHECK(funcall->name.compare("Const") == 0);
    CHECK(funcall->args.empty());
  }
  {
    std::string_view in = "Add(Const(), Const())";
    ns::Tokenizer it(in);
    auto result = ns::parse_expr(it);
    CHECK(result);
    auto funcall = std::get_if<ns::FunCall>(&*result);
    CHECK(funcall);
    CHECK(funcall->name.compare("Add") == 0);
    CHECK(funcall->args.size() == 2);
    for (const auto& expr2 : funcall->args) {
      auto funcall2 = std::get_if<ns::FunCall>(&expr2);
      CHECK(funcall2);
      CHECK(funcall2->name.compare("Const") == 0);
      CHECK(funcall2->args.empty());
    }
  }
  {
    std::string_view in = "Ident";
    ns::Tokenizer it(in);
    auto result = ns::parse_expr(it);
    CHECK(result);
    auto ident = std::get_if<ns::ASTIdent>(&*result);
    CHECK(ident);
    CHECK(ident->name.compare("Ident") == 0);
  }
  {
    std::string_view in = "Add(Ident, Ident)";
    ns::Tokenizer it(in);
    auto result = ns::parse_expr(it);
    CHECK(result);
    auto funcall = std::get_if<ns::FunCall>(&*result);
    CHECK(funcall);
    CHECK(funcall->name.compare("Add") == 0);
    CHECK(funcall->args.size() == 2);
    for (const auto& expr2 : funcall->args) {
      auto ident2 = std::get_if<ns::ASTIdent>(&expr2);
      CHECK(ident2);
      CHECK(ident2->name.compare("Ident") == 0);
    }
  }
}
