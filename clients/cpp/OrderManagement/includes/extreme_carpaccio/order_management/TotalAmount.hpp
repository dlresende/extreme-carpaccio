
#ifndef EXTREME_CARPACCIO_ORDER_MANAGEMENT_TOTALAMOUNT_HPP
#define EXTREME_CARPACCIO_ORDER_MANAGEMENT_TOTALAMOUNT_HPP

#include <extreme_carpaccio/order_management/config.hpp>

#include <boost/beast/http.hpp>

#include <string>
#include <utility>

namespace extreme_carpaccio {
namespace order_management {

using TotalAmountResponse = std::pair<boost::beast::http::status, double>;

EXTREME_CARPACCIO_ORDER_MANAGEMENT_API TotalAmountResponse computeTotalAmount(const std::string & requestBody);

} // namespace order_management
} // namespace extreme_carpaccio

#endif // EXTREME_CARPACCIO_ORDER_MANAGEMENT_TOTALAMOUNT_HPP

