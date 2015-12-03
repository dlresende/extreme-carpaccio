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

import static xcarpaccio.MyHttpServer.HttpResponse.ok;
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
        server.createContext("/order", new OrderHttpHandler());
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
        protected final ObjectMapper objectMapper = new ObjectMapper();
        @Override
        public void handle(HttpExchange httpExchange) throws IOException {
            HttpResponse response = doHandle(httpExchange);
            respond(httpExchange, response);
        }

        public abstract HttpResponse doHandle(HttpExchange request) throws IOException;

        private void respond(HttpExchange httpExchange, HttpResponse response) throws IOException {
            httpExchange.sendResponseHeaders(response.getStatusCode(), response.getBody().length);
            OutputStream os = httpExchange.getResponseBody();
            os.write(response.getBody());
            os.close();
        }
    }

    private class PingHttpHandler extends AbstractHttpHandler {
        @Override
        public HttpResponse doHandle(HttpExchange request) {
            return ok("pong");
        }
    }

    private class FeedbackHttpHandler extends AbstractHttpHandler {
        @Override
        public HttpResponse doHandle(HttpExchange request) {
            InputStream body = request.getRequestBody();

            try {
                FeedbackMessage message = objectMapper.readValue(body, FeedbackMessage.class);
                logger.log(message.getType() + ": " + message.getContent());
            } catch (IOException exception) {
                logger.error(exception.getMessage());
            }

            return ok();
        }
    }

    private class OrderHttpHandler extends AbstractHttpHandler {
        @Override
        public HttpResponse doHandle(HttpExchange request) throws IOException {
            String method = request.getRequestMethod();
            String uri = request.getRequestURI().getPath();
            String requestBody = stringify(request.getRequestBody());
            Order incomingOrder = objectMapper.readValue(requestBody, Order.class);
            logger.log(method + " " + uri + " " + incomingOrder.toString());

            double total = 42; // TODO compute me correctly or you'll get a penalty

            Result result = new Result(total);

            return ok(objectMapper.writeValueAsString(result)); // Use this to respond to an order with a total
//            return ok(""); // Use this if you don't want to respond to an order, without penalty
        }
    }

    public static class HttpResponse {
        private static final byte[] NO_CONTENT = new byte[]{};

        private final int statusCode;
        private final byte[] body;

        private HttpResponse(int statusCode, byte[] body) {
            this.statusCode = statusCode;
            this.body = body;
        }

        public static HttpResponse ok() {
            return ok(NO_CONTENT);
        }

        public static HttpResponse ok(byte[] body) {
            return new HttpResponse(200, body);
        }

        public static HttpResponse ok(String body) {
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

            HttpResponse response = (HttpResponse) o;

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
