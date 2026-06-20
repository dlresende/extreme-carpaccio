
#ifndef EXTREME_CARPACCIO_CLIENT_TEST_HELPERS_CARPACCIOSTREAM_HPP
#define EXTREME_CARPACCIO_CLIENT_TEST_HELPERS_CARPACCIOSTREAM_HPP

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

class CarpaccioStream
{
public:
   CarpaccioStream(const std::string & host, unsigned short port);
   ~CarpaccioStream();

   boost::beast::http::response<boost::beast::http::dynamic_body> read(boost::beast::flat_buffer&);
   void write(boost::beast::http::verb requestType, const std::string & target, const std::string & contentType, const std::string & body);

private:
   std::string m_serverHost;
   boost::asio::io_context m_ioContext;
   boost::asio::ip::tcp::resolver m_resolver;
   boost::asio::ip::tcp::resolver::results_type m_resolverResults;
   boost::beast::tcp_stream m_stream;
};

} // namespace test
} // namespace test
} // namespace client
} // namespace extreme_carpaccio

#endif // EXTREME_CARPACCIO_CLIENT_TEST_HELPERS_CARPACCIOSTREAM_HPP
