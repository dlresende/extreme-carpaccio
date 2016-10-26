package xcarpaccio

import org.wasabifx.wasabi.app.AppConfiguration
import org.wasabifx.wasabi.app.AppServer
import org.wasabifx.wasabi.routing.routeHandler

class MyServer(port: Int, val logger: Logger) {

    val server: AppServer
    init {
        server = AppServer(AppConfiguration(port))
    }

    val feedbackHandler = routeHandler {
        val type = request.bodyParams["type"]
        val content = request.bodyParams["content"]
        logger.log("$type: $content")
    }

    val orderHandler = routeHandler {
        logger.log("${request.method} ${request.uri} ${request.bodyParams}")
        val order = Order(request.bodyParams["prices"] as List<Double>,
                request.bodyParams["quantities"] as List<Double>,
                request.bodyParams["country"] as String,
                request.bodyParams["reduction"] as String)
        logger.log(order.toString())

        //TODO compute total, if your response contains the wrong total you'll get a penalty
//        response.send(Result(31.0), "application/json")
        response.send("")
    }

    fun start(wait: Boolean = true) {
        server.get("/ping", {
            response.send("pong")
        })

        server.post("/feedback", feedbackHandler)
        server.post("/order", orderHandler)

        server.start(wait)
    }

    fun shutdown() {
        server.stop()
    }

}

val PORT = 9000

fun main(args: Array<String>) {
    val logger = Logger()
    MyServer(PORT, logger).start(false)
    logger.log("Server running on port $PORT...")
}
