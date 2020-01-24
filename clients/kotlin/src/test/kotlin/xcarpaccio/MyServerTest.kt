package xcarpaccio

import io.ktor.http.ContentType
import io.ktor.http.HttpHeaders
import io.ktor.http.HttpMethod
import io.ktor.http.HttpStatusCode
import io.ktor.server.testing.handleRequest
import io.ktor.server.testing.setBody
import io.ktor.server.testing.withTestApplication
import kotlinx.serialization.UnstableDefault
import kotlinx.serialization.json.Json
import org.mockito.BDDMockito.then
import org.mockito.Mockito.mock
import kotlin.test.Test
import kotlin.test.assertEquals

class MyServerTest {
    val logger: Logger = mock(Logger::class.java)

    @Test
    fun testBasicGets() = withTestApplication({
        myModule(logger)
    }) {
        with(handleRequest(HttpMethod.Get, "/")) {
            assertEquals(HttpStatusCode.OK, response.status())
            assertEquals("Hello, world!", response.content)
        }
        with(handleRequest(HttpMethod.Get, "/ping")) {
            assertEquals(HttpStatusCode.OK, response.status())
            assertEquals("pong", response.content)
        }
    }

    @UnstableDefault
    @Test
    fun testFeedback() = withTestApplication({
        myModule(logger)
    }) {
        with(handleRequest(HttpMethod.Post, "/feedback") {
            addHeader(HttpHeaders.ContentType, ContentType.Application.Json.toString())
            val feedback = Feedback("ERROR", "The field \"total\" in the response is missing.")
            val json = Json.indented.stringify(Feedback.serializer(), feedback)
            setBody(json)
        }) {
            assertEquals(HttpStatusCode.OK, response.status())
            assertEquals("", response.content)
            then(logger).should().log("""ERROR: The field "total" in the response is missing.""")
        }
    }

    @UnstableDefault
    @Test
    fun testOrder() = withTestApplication({
        myModule(logger)
    }) {
        with(handleRequest(HttpMethod.Post, "/order") {
            addHeader(HttpHeaders.ContentType, ContentType.Application.Json.toString())
            val order = Order(
                    prices = listOf(3.5),
                    quantities = listOf(2.0),
                    country = "FR",
                    reduction = "STANDARD")
            val json = Json.indented.stringify(Order.serializer(), order)
            setBody(json)
        }) {
            then(logger).should().log("order {\"prices\":[3.5],\"quantities\":[2.0],\"country\":\"FR\",\"reduction\":\"STANDARD\"}")
            assertEquals(HttpStatusCode.OK, response.status())
            assertEquals("", response.content)
        }
    }
}