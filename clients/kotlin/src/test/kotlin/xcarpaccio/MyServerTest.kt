package xcarpaccio

import io.ktor.client.request.post
import io.ktor.client.statement.bodyAsText
import io.ktor.http.HttpStatusCode
import io.ktor.server.testing.testApplication
import kotlin.test.Test
import kotlin.test.assertEquals

class MyServerTest {
    @Test
    fun postPingRespondsWithPong() = testApplication {
        application { appModule() }

        val response = client.post("/ping")

        assertEquals(HttpStatusCode.OK, response.status)
        assertEquals("pong", response.bodyAsText())
    }
}
