package extremecarpaccio;

import static spark.Spark.awaitInitialization;
import static spark.Spark.port;
import static spark.Spark.post;
import static spark.Spark.stop;

public final class HttpServer {
    private HttpServer() {
    }

    public static void start(int portNumber) {
        port(portNumber);
        post("/ping", (request, response) -> "pong");
        awaitInitialization();
    }

    public static void stopServer() {
        stop();
    }

    public static void main(String[] args) {
        start(Integer.parseInt(System.getenv().getOrDefault("PORT", "3000")));
    }
}
