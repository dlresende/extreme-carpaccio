
#include <extreme_carpaccio_client/ExtremeCarpaccioClient.hpp>

int main()
{
   extreme_carpaccio_client::CarpaccioServer server(8081);
   server.start();
}