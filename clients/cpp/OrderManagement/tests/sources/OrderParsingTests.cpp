
#include <extreme_carpaccio/order_management/Order.hpp>
#include <extreme_carpaccio/order_management/OrderParsing.hpp>

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <string>

using namespace testing;

namespace extreme_carpaccio {
namespace order_management {
namespace test {

TEST(OrderParsing, should_return_order_object_from_json_order)
{
   std::string orderRequest = "{\"prices\": [1,1.5], \"quantities\": [100, 200], \"country\": \"DE\", \"reduction\": \"STANDARD\"}";

   Order order = parseOrder(orderRequest);

   EXPECT_EQ("DE", order.country);
   EXPECT_EQ("STANDARD", order.reduction);

   EXPECT_THAT(order.prices, ElementsAre(1, 1.5));
   EXPECT_THAT(order.quantities, ElementsAre(100, 200));
}

} // namespace tests
} // namespace order_management
} // namespace extreme_management
