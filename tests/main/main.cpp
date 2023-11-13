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
