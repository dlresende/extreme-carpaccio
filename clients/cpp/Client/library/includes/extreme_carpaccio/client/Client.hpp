
#ifndef EXTREME_CARPACCIO_CLIENT_CLIENT_HPP
#define EXTREME_CARPACCIO_CLIENT_CLIENT_HPP

#include <extreme_carpaccio/client/config.hpp>

#include <boost/asio.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <boost/beast/version.hpp>

#include <string>

namespace extreme_carpaccio {
namespace client {

class http_worker
{
   using alloc_t = std::allocator<char>;
   using request_body_t = boost::beast::http::string_body;

public:
   http_worker(http_worker const&) = delete;
   http_worker& operator=(http_worker const&) = delete;

   http_worker(boost::asio::ip::tcp::acceptor& acceptor, const std::string& doc_root);

   void start();

private:
   void accept();

   void read_request();

   void process_request(boost::beast::http::request<request_body_t, boost::beast::http::basic_fields<alloc_t>> const& req);

   void send_response(boost::beast::http::status status, std::string const& body);

   void check_deadline();

   bool handleRequest(boost::beast::http::verb requestType, const std::string & target, const std::string & contentType, const std::string & body);

private:
   // The acceptor used to listen for incoming connections.
   boost::asio::ip::tcp::acceptor& m_acceptor;

   // The path to the root of the document directory.
   std::string m_doc_root;

   // The socket for the currently connected client.
   boost::asio::ip::tcp::socket m_socket;

   // The buffer for performing reads
   boost::beast::flat_static_buffer<8192> m_buffer;

   // The allocator used for the fields in the request and reply.
   alloc_t m_alloc;

   // The parser for reading the requests
   boost::optional<boost::beast::http::request_parser<request_body_t, alloc_t>> m_parser;

   // The timer putting a time limit on requests.
   boost::asio::steady_timer m_request_deadline;

   // The string-based response message.
   boost::optional<boost::beast::http::response<boost::beast::http::string_body, boost::beast::http::basic_fields<alloc_t>>> m_string_response;

   // The file-based response message.
   boost::optional<boost::beast::http::response<boost::beast::http::file_body, boost::beast::http::basic_fields<alloc_t>>> m_file_response;
};

class EXTREME_CARPACCIO_CLIENT_API CarpaccioServer
{
public:
   CarpaccioServer(unsigned short port);
   CarpaccioServer();
   void start();
   void stop();

private:
   boost::asio::io_context ioc;
   boost::asio::ip::tcp::acceptor acceptor;
   http_worker worker;
};

} // namespace client
} // namespace extreme_carpaccio

#endif // EXTREME_CARPACCIO_CLIENT_CLIENT_HPP
