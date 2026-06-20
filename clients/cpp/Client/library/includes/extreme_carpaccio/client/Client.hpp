
#ifndef EXTREME_CARPACCIO_CLIENT_CLIENT_HPP
#define EXTREME_CARPACCIO_CLIENT_CLIENT_HPP

#include <extreme_carpaccio/client/CarpaccioHttpWorker.hpp>
#include <extreme_carpaccio/client/config.hpp>

#include <boost/asio.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <boost/beast/version.hpp>

#include <string>

namespace extreme_carpaccio {
namespace client {

class EXTREME_CARPACCIO_CLIENT_API CarpaccioServer
{
public:
   CarpaccioServer(const std::string& ip, unsigned short port);
   void start();
   void stop();

private:
   boost::asio::io_context ioc;
   boost::asio::ip::tcp::acceptor acceptor;
   CarpaccioHttpWorker worker;
};

} // namespace client
} // namespace extreme_carpaccio

#endif // EXTREME_CARPACCIO_CLIENT_CLIENT_HPP
