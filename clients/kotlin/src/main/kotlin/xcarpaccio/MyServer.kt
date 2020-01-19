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
import kotlinx.serialization.UnstableDefault
import kotlinx.serialization.json.Json

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

@UnstableDefault
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
        post("/order") {
            val order = call.receive<Order>()
            logger.log("order ${Json.plain.stringify(Order.serializer(), order)}")
            call.respond(HttpStatusCode.OK, "")
        }
    }
}

@Serializable
data class Feedback(val type: String, val content: String) {
}
