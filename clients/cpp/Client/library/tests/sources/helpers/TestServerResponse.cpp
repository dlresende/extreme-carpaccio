

#include <helpers/TestServerResponse.hpp>
#include <helpers/CarpaccioStream.hpp>

#include <extreme_carpaccio/client/Client.hpp>
#include <extreme_carpaccio/client/HttpConfig.hpp>

#include <boost/asio.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <boost/beast/version.hpp>

#include <thread>
#include <string>

namespace extreme_carpaccio {
namespace client {
namespace test {
namespace helpers {

namespace beast = boost::beast;
namespace http = beast::http;
namespace net = boost::asio;
using tcp = boost::asio::ip::tcp;

namespace {

const char serverHost[] = "localhost";
const unsigned short testServerPort = DEFAULT_HTTP_SERVER_PORT + 1;

} // namespace

boost::beast::http::response<boost::beast::http::dynamic_body> generateServerResponse(
   boost::beast::http::verb requestType
   , const std::string & target
   , const std::string & contentType
   , const std::string & body
)
{
   CarpaccioServer server(DEFAULT_HTTP_SERVER_IP, testServerPort);
   std::thread thread(&CarpaccioServer::start, &server);
   std::this_thread::sleep_for(std::chrono::seconds(1));

   CarpaccioStream stream(serverHost, testServerPort);

   // Send the HTTP request to the remote host
   stream.write(requestType, target, contentType, body);

   // This buffer is used for reading and must be persisted
   beast::flat_buffer buffer;

   // Receive the HTTP response
   auto res = stream.read(buffer);

   server.stop();

   thread.detach();

   return res;
}

} // namespace helpers
} // namespace test
} // namespace client
} // namespace extreme_carpaccio
