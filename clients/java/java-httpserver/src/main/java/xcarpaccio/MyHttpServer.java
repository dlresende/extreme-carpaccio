package xcarpaccio;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;
import org.codehaus.jackson.map.ObjectMapper;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.util.Arrays;

import static xcarpaccio.MyHttpServer.Response.ok;
import static xcarpaccio.StringUtils.stringify;

public class MyHttpServer
{
    private final int port;
    private final Logger logger;

    private HttpServer server;

    public MyHttpServer(int port, Logger logger) {
        this.port = port;
        this.logger = logger;
    }

    public void start() throws IOException {
        server = HttpServer.create(new InetSocketAddress(port), 0);
        server.createContext("/ping", new PingHttpHandler());
        server.createContext("/feedback", new FeedbackHttpHandler());
        server.createContext("/", new ConsoleHttpHandler());
        server.start();

        logger.log("Server running on port " + port + "...");
    }

    public void shutdown() {
        if(server != null) {
            logger.log("Stopping server...");
            server.stop(2);
        }
    }

    public static void main( String[] args ) throws IOException {
        Logger logger = new Logger();
        new MyHttpServer(9000, logger).start();
    }

    private abstract class AbstractHttpHandler implements HttpHandler {
        @Override
        public void handle(HttpExchange httpExchange) throws IOException {
            Response response = doHandle(httpExchange);
            respond(httpExchange, response);
        }

        public abstract Response doHandle(HttpExchange request);

        private void respond(HttpExchange httpExchange, Response response) throws IOException {
            httpExchange.sendResponseHeaders(response.getStatusCode(), response.getBody().length);
            OutputStream os = httpExchange.getResponseBody();
            os.write(response.getBody());
            os.close();
        }
    }

    private class PingHttpHandler extends AbstractHttpHandler {
        @Override
        public Response doHandle(HttpExchange request) {
            return ok("pong");
        }
    }

    private class FeedbackHttpHandler extends AbstractHttpHandler {
        protected final ObjectMapper objectMapper = new ObjectMapper();

        @Override
        public Response doHandle(HttpExchange request) {
            InputStream body = request.getRequestBody();

            try {
                Message message = objectMapper.readValue(body, Message.class);
                logger.log(message.getType() + ": " + message.getContent());
            } catch (IOException exception) {
                logger.error(exception.getMessage());
            }

            return ok();
        }
    }

    private class ConsoleHttpHandler extends AbstractHttpHandler {
        @Override
        public Response doHandle(HttpExchange request) {
            String method = request.getRequestMethod();
            String uri = request.getRequestURI().getPath();
            logger.log(method + " " + uri + " " + stringify(request.getRequestBody()));
            return ok();
        }
    }

    public static class Response {
        private static final byte[] NO_CONTENT = new byte[]{};

        private final int statusCode;
        private final byte[] body;

        private Response(int statusCode, byte[] body) {
            this.statusCode = statusCode;
            this.body = body;
        }

        public static Response ok() {
            return ok(NO_CONTENT);
        }

        public static Response ok(byte[] body) {
            return new Response(200, body);
        }

        public static Response ok(String body) {
            return ok(body != null ? body.getBytes() : NO_CONTENT);
        }

        public int getStatusCode() {
            return statusCode;
        }

        public byte[] getBody() {
            return body;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Response response = (Response) o;

            return statusCode == response.statusCode && Arrays.equals(body, response.body);
        }

        @Override
        public int hashCode() {
            int result = statusCode;
            result = 31 * result + (body != null ? Arrays.hashCode(body) : 0);
            return result;
        }
    }
}
