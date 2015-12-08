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
                post("/order", (context -> {
                    String method = context.method();
                    String uri = context.uri();
                    String body = context.extract(String.class);
                    logger.log(method + " " + uri + " " + body);
                    Order order = context.extract(Order.class);
                    logger.log("Unserialized order: " + order);

                    // Use the following line to choose not to handle an order
                    return new Payload("application/json", "", 200);

                    // Use the following lines to return a total:
//                    double total = 42.0;
//                    Answer answer = new Answer(total);
//                    return new Payload("application/json", answer, 200);
                }))
        ;
    }
}
