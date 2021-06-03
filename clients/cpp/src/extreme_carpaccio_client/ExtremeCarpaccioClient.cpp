
#include <extreme_carpaccio_client/ExtremeCarpaccioClient.hpp>

#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <boost/beast/version.hpp>
#include <boost/asio.hpp>
#include <boost/filesystem.hpp>
#include <chrono>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <list>
#include <memory>
#include <string>

#include <cstdlib>

namespace ip = boost::asio::ip;         // from <boost/asio.hpp>
using tcp = boost::asio::ip::tcp;       // from <boost/asio.hpp>
namespace http = boost::beast::http;    // from <boost/beast/http.hpp>

namespace extreme_carpaccio_client {

   int launchServer()
   {
      auto const address = boost::asio::ip::make_address("localhost");
      unsigned short port = static_cast<unsigned short>(std::atoi("8081"));
      std::string doc_root = "./feedback";
      int num_workers = std::atoi("1");
      bool spin = true;

      boost::asio::io_context ioc{ 1 };
      tcp::acceptor acceptor{ ioc, {address, port} };



      return EXIT_SUCCESS;
   }


} // namespace extreme_carpaccio_client

