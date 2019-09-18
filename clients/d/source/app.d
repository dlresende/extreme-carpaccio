import vibe.d;

import models.order;
import models.feedback;

import utils;

void main()
{
    auto router = new URLRouter();
    router.post("/order", &handleOrder);
    router.post("/feedback", &handleFeedback);

    auto settings = new HTTPServerSettings(":9000");

    // Starting the server
    listenHTTP(settings, router);
    runApplication();
}

/**
 * Handling POST /order request.
 */
void handleOrder(HTTPServerRequest request, HTTPServerResponse response)
{
    info("Received order JSON: %s", request.json);

    auto order = deserializeJson!Order(request.json);

    throw new HTTPStatusException(HTTPStatus.notFound);
}

/**
 * Handling POST /feedback request.
 */
void handleFeedback(HTTPServerRequest request, HTTPServerResponse response)
{
    auto feedback = deserializeJson!Feedback(request.json);

    error("Received feedback %s: %s", feedback.type, feedback.content);
}