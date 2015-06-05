package xcarpaccio;

import net.codestory.http.WebServer;
import net.codestory.http.payload.Payload;

public class MyFluentHttpServer {

    private final Logger logger;

    public MyFluentHttpServer() {
        logger = new Logger();

        new WebServer().
                configure(routes -> routes.
                                get("/ping", "pong").
                                post("/feedback", (context) -> {
                                    Message message = context.extract(Message.class);
                                    logger.log(message.type + ": " + message.content);
                                    return new Payload(204);
                                }).
                                post("/", (context) -> {
                                    String method = context.method();
                                    String uri = context.uri();
                                    String body = context.extract(String.class);
                                    logger.log(method + " " + uri + " " + body);
                                    return new Payload(204);
                                })
                ).
                start(9000);
    }

    public static void main( String[] args ) {
        new MyFluentHttpServer();
    }
}
