package katazon;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spark.ResponseTransformer;
import spark.Spark;

import static spark.Spark.get;
import static spark.Spark.post;
import static spark.SparkBase.port;

public class HttpServer {
    private static final Logger logger = LoggerFactory.getLogger(HttpServer.class);

    public static void main(String[] args) {
        start(1337);
    }

    public static void start(int port) {
        port(port);
        configureRoutes();
    }

    public static void stop() {
        Spark.stop();
    }

    private static void configureRoutes() {
        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        ResponseTransformer asJson = new JsonResponseTransformer(gson);

        get("/hello", (req, res) -> "Hello World");
        post("/ping", (req, res) -> "pong", asJson);
        post("/", (req, res) -> {
            JsonObject body = gson.fromJson(req.body(), JsonObject.class);
            logger.info("Incoming request on '/': {}", body.entrySet());
            return "pong";
        }, asJson);
    }

}
