
#include <extreme_carpaccio/order_management/TotalAmount.hpp>

#include <gtest/gtest.h>

using namespace testing;

namespace beast = boost::beast;
namespace http = beast::http;

namespace extreme_carpaccio {
namespace order_management {
namespace test {

TEST(TotalAmount, dummy_test)
{
   const std::string requestBody = "{\"prices\": [1,1.5], \"quantities\": [100, 200], \"country\": \"DE\", \"reduction\": \"STANDARD\"}";
   auto response = computeTotalAmount(requestBody);

   EXPECT_EQ(response.first, http::status::not_found);
   EXPECT_EQ(response.second, 42.);
}

} // namespace tests
} // namespace order_management
} // namespace extreme_management
