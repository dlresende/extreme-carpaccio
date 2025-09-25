
#include <extreme_carpaccio/client/Client.hpp>
#include <extreme_carpaccio/client/HttpConfig.hpp>

#include <boost/asio.hpp>

namespace extreme_carpaccio {
namespace client {

CarpaccioServer::CarpaccioServer(const std::string& ip, unsigned short port)
   : ioc(1)
   , acceptor(ioc, { boost::asio::ip::make_address(ip), port })
   , worker(acceptor, "./feedback")
{
}

void CarpaccioServer::start()
{
   worker.start();
   ioc.run();
}

void CarpaccioServer::stop()
{
   ioc.stop();
}
   
} // namespace client
} // namespace extreme_carpaccio
