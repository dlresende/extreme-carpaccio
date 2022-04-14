
#include <extreme_carpaccio/order_management/TotalAmount.hpp>

#include <gtest/gtest.h>

using namespace testing;

namespace extreme_carpaccio {
namespace order_management {
namespace test {

TEST(TotalAmount, dummy_test)
{
   Order order;
   EXPECT_EQ(42., computeTotalAmount(order));
}

} // namespace tests
} // namespace order_management
} // namespace extreme_management
