
#include <extreme_carpaccio/client/Client.hpp>

#include <cstdlib>

int main()
{
   extreme_carpaccio::client::CarpaccioServer server;
   server.start();

   return EXIT_SUCCESS;
}
