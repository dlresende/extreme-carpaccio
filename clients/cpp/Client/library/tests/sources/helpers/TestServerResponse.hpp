
#ifndef EXTREME_CARPACCIO_CLIENT_TEST_HELERS_TESTSERVERRESPONSE_HPP
#define EXTREME_CARPACCIO_CLIENT_TEST_HELERS_TESTSERVERRESPONSE_HPP

#include <extreme_carpaccio/client/config.hpp>

#include <boost/asio.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <boost/beast/version.hpp>

#include <string>

namespace extreme_carpaccio {
namespace client {
namespace test {
namespace helpers {

boost::beast::http::response<boost::beast::http::dynamic_body> generateServerResponse(
     boost::beast::http::verb requestType
   , const std::string & target
   , const std::string & contentType = ""
   , const std::string & body = ""
);

} // namespace helpers
} // namespace test
} // namespace client
} // namespace extreme_carpaccio

#endif // EXTREME_CARPACCIO_CLIENT_TEST_HELERS_TESTSERVERRESPONSE_HPP
