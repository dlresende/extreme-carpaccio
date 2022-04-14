
#ifndef EXTREME_CARPACCIO_ORDER_MANAGEMENT_ORDER_HPP
#define EXTREME_CARPACCIO_ORDER_MANAGEMENT_ORDER_HPP

#include <extreme_carpaccio/order_management/config.hpp>

#include <ostream>
#include <string>
#include <vector>

namespace extreme_carpaccio {
namespace order_management {

struct EXTREME_CARPACCIO_ORDER_MANAGEMENT_API Order
{
   std::vector<unsigned int> quantities;
   std::vector<double> prices;
   std::string country;
   std::string reduction;
};

EXTREME_CARPACCIO_ORDER_MANAGEMENT_API std::ostream& operator<< (std::ostream& stream, const Order& order);

} // namespace order_management
} // namespace extreme_carpaccio

#endif // EXTREME_CARPACCIO_ORDER_MANAGEMENT_ORDER_HPP