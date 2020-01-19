package xcarpaccio

import io.ktor.application.Application
import io.ktor.application.call
import io.ktor.application.install
import io.ktor.features.ContentNegotiation
import io.ktor.http.ContentType
import io.ktor.http.HttpStatusCode
import io.ktor.request.receive
import io.ktor.response.respond
import io.ktor.response.respondText
import io.ktor.routing.get
import io.ktor.routing.post
import io.ktor.routing.routing
import io.ktor.serialization.DefaultJsonConfiguration
import io.ktor.serialization.serialization
import io.ktor.server.engine.embeddedServer
import io.ktor.server.netty.Netty
import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json

/*
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
*/


val PORT = 9000

fun main(args: Array<String>) {
    val logger = Logger()

    logger.log("Server running on port $PORT...")
    embeddedServer(
            Netty,
            port = PORT,
            module = {
                myModule(logger)
            }
    ).start(wait = true)
}

fun Application.myModule(logger: Logger) {
    install(ContentNegotiation) {
        serialization(
                contentType = ContentType.Application.Json,
                json = Json(
                        DefaultJsonConfiguration.copy(
                                prettyPrint = true
                        )
                )
        )
    }
    routing {
        get("/") {
            call.respondText("Hello, world!", ContentType.Text.Plain)
        }
        get("/ping") {
            call.respondText("pong", ContentType.Text.Plain)
        }
        post("/feedback") {
            val feedback = call.receive<Feedback>()
            call.respond(HttpStatusCode.OK, "")
            logger.log("${feedback.type}: ${feedback.content}")
        }
    }
}


@Serializable
data class Feedback(val type: String, val content: String) {
}
