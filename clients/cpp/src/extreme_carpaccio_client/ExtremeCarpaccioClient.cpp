
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

class http_worker
{
public:
http_worker(tcp::acceptor& acceptor, const std::string& doc_root)
   : m_acceptor(acceptor)
   , m_doc_root(doc_root)
   , m_socket(m_acceptor.get_executor())
{
}

void send_bad_response(
        http::status status,
        std::string const& error)
{
   http::response<http::string_body> string_response_;

   string_response_.result(status);
   string_response_.keep_alive(false);
   string_response_.set(http::field::server, "Beast");
   string_response_.set(http::field::content_type, "text/plain");
   string_response_.body() = error;
   string_response_.prepare_payload();

   http::response_serializer<http::string_body> string_serializer(string_response_);

   http::async_write(
      m_socket,
      string_serializer,
      [this](boost::beast::error_code ec, std::size_t)
      {
         m_socket.shutdown(tcp::socket::shutdown_send, ec);
         accept();
      });
}


void read_request()
{
   // On each read the parser needs to be destroyed and
   // recreated. We store it in a boost::optional to
   // achieve that.
   //
   // Arguments passed to the parser constructor are
   // forwarded to the message object. A single argument
   // is forwarded to the body constructor.
   //
   // We construct the dynamic body with a 1MB limit
   // to prevent vulnerability to buffer attacks.
   //

   //http::request_parser<http::string_body> parser(std::piecewise_construct, std::make_tuple());

   //http::async_read(
   //   m_socket,
   //   m_buffer,
   //   parser,
   //   [this](boost::beast::error_code ec, std::size_t)
   //   {
         send_bad_response(http::status::bad_request, "Invalid request-method");
      //});
}

void accept()
{
   m_acceptor.async_accept(
      m_socket,
      [this](boost::beast::error_code ec)
      {
         read_request();
      });

}

void start()
{
   accept();
}

private:
   tcp::acceptor& m_acceptor;
   const std::string& m_doc_root;
   tcp::socket m_socket;
   boost::beast::flat_static_buffer<8192> m_buffer;
};


namespace extreme_carpaccio_client {

   int launchServer()
   {
      auto const address = boost::asio::ip::make_address("127.0.0.1");
      unsigned short port = static_cast<unsigned short>(std::atoi("8081"));
      std::string doc_root = "./feedback";
      //int num_workers = std::atoi("1");
      //bool spin = true;

      boost::asio::io_context ioc{ 1 };
      tcp::acceptor acceptor{ ioc, {address, port} };

      http_worker worker(acceptor, doc_root);
      worker.start();

      ioc.run();

      return EXIT_SUCCESS;
   }


} // namespace extreme_carpaccio_client

