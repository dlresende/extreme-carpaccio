
#include <extreme_carpaccio/order_management/TotalAmount.hpp>

namespace extreme_carpaccio {
namespace order_management {

namespace beast = boost::beast;
namespace http = beast::http;

TotalAmountResponse::TotalAmountResponse(boost::beast::http::status status, double totalAmount)
   : m_status(status)
   , m_totalAmount(totalAmount)
{
}


TotalAmountResponse computeTotalAmount(const std::string & /*requestBody*/)
{
   return TotalAmountResponse(http::status::not_found, 42.);
}

} // namespace order_management
} // namespace extreme_carpaccio
