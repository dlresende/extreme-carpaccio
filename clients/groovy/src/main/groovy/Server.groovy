import com.sun.net.httpserver.HttpHandler
import com.sun.net.httpserver.HttpServer
import java.nio.charset.StandardCharsets

class Server implements Closeable {

    private final HttpServer server

    Server(int port) {
        server = HttpServer.create(new InetSocketAddress(port), 0)
        server.createContext('/ping', { exchange ->
            if (exchange.requestMethod != 'POST') {
                exchange.sendResponseHeaders(405, -1)
                exchange.close()
                return
            }

            byte[] body = 'pong'.getBytes(StandardCharsets.UTF_8)
            exchange.responseHeaders.set('Content-Type', 'text/plain; charset=utf-8')
            exchange.sendResponseHeaders(200, body.length)

            OutputStream out = exchange.responseBody
            try {
                out.write(body)
            } finally {
                out.close()
            }
        } as HttpHandler)
    }

    void start() {
        server.start()
    }

    int port() {
        server.address.port
    }

    @Override
    void close() {
        server.stop(0)
    }

    static void main(String[] args) {
        int port = System.getenv('PORT') ? System.getenv('PORT').toInteger() : 3000
        def server = new Server(port)
        server.start()
        println "Groovy client listening on port ${port}"
    }
}
