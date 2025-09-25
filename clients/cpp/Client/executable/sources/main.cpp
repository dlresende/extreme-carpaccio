
#include <extreme_carpaccio/client/Client.hpp>
#include <extreme_carpaccio/client/HttpConfig.hpp>

#include <cstdlib>
#include <iostream>

int main(int argc, char *argv[])
{
   std::string ip = DEFAULT_HTTP_SERVER_IP;
   unsigned short port = DEFAULT_HTTP_SERVER_PORT;

   if (argc == 3)
   {
      ip = argv[1];
      port = std::atoi(argv[2]);
   }
   std::cout << "Extreme Carpaccio server starting with ip " << ip << " and port " << port << "." << std::endl;
   extreme_carpaccio::client::CarpaccioServer server(ip, port);
   server.start();
   std::cout << "Extreme Carpaccio server stopping." << std::endl;

   return EXIT_SUCCESS;
}
