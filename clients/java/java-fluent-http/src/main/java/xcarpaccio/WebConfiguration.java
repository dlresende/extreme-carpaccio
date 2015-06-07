package xcarpaccio;

import net.codestory.http.Configuration;
import net.codestory.http.payload.Payload;
import net.codestory.http.routes.Routes;

public class WebConfiguration implements Configuration {

    private final Logger logger = new Logger();

    @Override
    public void configure(Routes routes) {
        routes.
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
        ;
    }
}
