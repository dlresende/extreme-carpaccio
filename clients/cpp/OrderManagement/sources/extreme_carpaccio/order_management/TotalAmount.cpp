
#include <extreme_carpaccio/order_management/TotalAmount.hpp>

namespace beast = boost::beast;
namespace http = beast::http;

namespace extreme_carpaccio {
namespace order_management {

TotalAmountResponse computeTotalAmount(const std::string & /*requestBody*/)
{
   return std::make_pair(http::status::not_found, 42.);
}

} // namespace order_management
} // namespace extreme_carpaccio
