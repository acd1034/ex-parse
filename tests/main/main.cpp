#include <catch2/catch_test_macros.hpp>
#include <iris/fundamental.hpp>

TEST_CASE("main", "[main][similar_to]") {
  CHECK(iris::similar_to<const int&, int>);
}
