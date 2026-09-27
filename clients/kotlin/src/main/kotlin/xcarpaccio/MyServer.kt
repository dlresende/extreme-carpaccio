package xcarpaccio

import io.ktor.http.ContentType
import io.ktor.server.application.Application
import io.ktor.server.application.call
import io.ktor.server.engine.embeddedServer
import io.ktor.server.netty.Netty
import io.ktor.server.response.respondText
import io.ktor.server.routing.post
import io.ktor.server.routing.routing

fun main() {
    val port = System.getenv("PORT")?.toIntOrNull() ?: 3000
    embeddedServer(Netty, host = "0.0.0.0", port = port, module = { appModule() })
        .start(wait = true)
}

fun Application.appModule() {
    routing {
        post("/ping") {
            call.respondText("pong", ContentType.Text.Plain)
        }
    }
}
