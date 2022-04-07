#include <extreme_carpaccio_client/config.hpp>

#include <string>
#include <vector>

namespace extreme_carpaccio_client {

struct EXTREME_CARPACCIO_CLIENT_API Order
{
   std::vector<unsigned int> quantities;
   std::vector<double> prices;
   std::string country;
   std::string reduction;
};

EXTREME_CARPACCIO_CLIENT_API Order parseOrder(const std::string& jsonOrder);
EXTREME_CARPACCIO_CLIENT_API std::ostream& operator<< (std::ostream& stream, const Order& order);

} // namespace extreme_carpaccio_client
