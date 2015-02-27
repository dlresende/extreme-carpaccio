package xcarpaccio;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;
import org.codehaus.jackson.map.ObjectMapper;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;

import static xcarpaccio.StringUtils.stringify;

public class MyHttpServer
{
    private final int port;
    private final Logger logger;
    private HttpServer server;

    MyHttpServer(int port) {
        this.port = port;
        this.logger = new Logger();
    }

    MyHttpServer(int port, Logger logger) {
        this.port = port;
        this.logger = logger;
    }

    void start() throws IOException {
        server = HttpServer.create(new InetSocketAddress(port), 0);
        server.createContext("/", new ConsoleHttpHandler());
        server.createContext("/ping", new PingHttpHandler());
        server.createContext("/feedback", new FeedbackHttpHandler());
        server.start();

        logger.log("Server running on port " + port + "...");
    }

    void shutdown() {
        if(server != null) {
            logger.log("Stopping server...");
            server.stop(2);
        }
    }

    public static void main( String[] args ) throws IOException {
        new MyHttpServer(9000).start();
    }

    private abstract class AbstractHttpHandler implements HttpHandler {
        protected static final String NO_CONTENT = "";

        @Override
        public void handle(HttpExchange httpExchange) throws IOException {
            String response = respond(httpExchange);
            httpExchange.sendResponseHeaders(200, response.length());
            OutputStream os = httpExchange.getResponseBody();
            os.write(response.getBytes());
            os.close();
        }

        public abstract String respond(HttpExchange httpExchange);
    }

    private class PingHttpHandler extends AbstractHttpHandler {
        @Override
        public String respond(HttpExchange httpExchange) {
            return "pong";
        }
    }

    private class FeedbackHttpHandler extends AbstractHttpHandler {
        protected final ObjectMapper objectMapper = new ObjectMapper();

        @Override
        public String respond(HttpExchange httpExchange) {
            InputStream body = httpExchange.getRequestBody();

            try {
                Message message = objectMapper.readValue(body, Message.class);
                logger.log(message.getType() + ": " + message.getContent());
            } catch (IOException exception) {
                logger.error(exception.getMessage());
            }

            return NO_CONTENT;
        }
    }

    private class ConsoleHttpHandler extends AbstractHttpHandler {


        @Override
        public String respond(HttpExchange httpExchange) {
            logger.log(stringify(httpExchange.getRequestBody()));
            return NO_CONTENT;
        }
    }
}
