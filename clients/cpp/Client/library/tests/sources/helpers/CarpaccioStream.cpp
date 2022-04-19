
#include <helpers/CarpaccioStream.hpp>

#include <boost/asio.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <boost/beast/version.hpp>

namespace extreme_carpaccio {
namespace client {
namespace test {
namespace helpers {

namespace beast = boost::beast;
namespace http = beast::http;
namespace net = boost::asio;
using tcp = boost::asio::ip::tcp;

namespace {

const int version = 11;

} // namespace

CarpaccioStream::CarpaccioStream(const std::string & host, unsigned short port)
   : m_serverHost(host)
   , m_ioContext()
   , m_resolver(m_ioContext) // These objects perform our I/O
   , m_resolverResults(m_resolver.resolve(host, std::to_string(port))) // Look up the domain name
   , m_stream(m_ioContext)
{
   // Make the connection on the IP address we get from a lookup
   m_stream.connect(m_resolverResults);
}

CarpaccioStream::~CarpaccioStream()
{
   // Gracefully close the socket
   beast::error_code ec;
   m_stream.socket().shutdown(tcp::socket::shutdown_both, ec);

   //// not_connected happens sometimes
   //// so don't bother reporting it.
   //if (ec && ec != beast::errc::not_connected)
   //   throw beast::system_error{ ec };
}

http::response<http::dynamic_body> CarpaccioStream::read(beast::flat_buffer & buffer)
{
   // Declare a container to hold the response
   http::response<http::dynamic_body> response;

   // Receive the HTTP response
   http::read(m_stream, buffer, response);

   return response;
}

void CarpaccioStream::write(boost::beast::http::verb requestType, const std::string & target, const std::string & contentType, const std::string & body)
{
   // Set up an HTTP GET request message
   http::request<http::string_body> request{ requestType, target, version };
   request.set(http::field::host, m_serverHost);
   request.set(http::field::user_agent, BOOST_BEAST_VERSION_STRING);
   request.set(http::field::content_type, contentType);
   request.body() = body;
   request.prepare_payload();

   // Send the HTTP request to the remote host
   http::write(m_stream, request);
}
   
} // namespace helpers
} // namespace test
} // namespace client
} // namespace extreme_carpaccio
