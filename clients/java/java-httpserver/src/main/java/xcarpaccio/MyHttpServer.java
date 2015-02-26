package xcarpaccio;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;

public class MyHttpServer
{
    private final int port;
    private HttpServer server;

    MyHttpServer(int port) {
        this.port = port;
    }

    void start() throws IOException {
        server = HttpServer.create(new InetSocketAddress(port), 0);
        server.createContext("/ping", new PingHttpHandler());
        server.start();

        System.out.println("Server running on port " + port + "...");
    }

    void shutdown() {
        if(server != null) {
            System.out.println("Stopping server...");
            server.stop(2);
        }
    }

    public static void main( String[] args ) throws IOException {
        new MyHttpServer(9000).start();
    }

    private abstract class AbstractHttpHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange httpExchange) throws IOException {
            String response = respond(httpExchange);
            httpExchange.sendResponseHeaders(200, response.length());
            OutputStream os = httpExchange.getResponseBody();
            os.write(response.getBytes());
            os.close();
            System.out.println(httpExchange.getRequestURI() + " " + response);
        }

        public abstract String respond(HttpExchange httpExchange);
    }

    private class PingHttpHandler extends AbstractHttpHandler {
        @Override
        public String respond(HttpExchange httpExchange) {
            return "pong";
        }
    }
}
