#include <extreme_carpaccio/order_management/Order.hpp>

#include <cstdlib>

namespace extreme_carpaccio {
namespace order_management {

template <class T>
static std::ostream& operator<< (std::ostream& stream, const std::vector<T>& v)
{
   stream << "{";
   for (std::size_t i = 0; i < v.size(); ++i)
   {
      if (i > 0)
      {
         stream << ",";
      }
      stream << v[i];
   }
   stream << '}';
   return stream;
}

std::ostream& operator<< (std::ostream& stream, const Order& order)
{
	stream << "Order{" <<
		"prices=" << order.prices <<
		", quantities=" << order.quantities <<
		", country='" << order.country + '\'' <<
		", reduction='" << order.reduction << '\'' <<
		'}';
	return stream;
}

} // namespace order_management
} // namespace extreme_carpaccio
