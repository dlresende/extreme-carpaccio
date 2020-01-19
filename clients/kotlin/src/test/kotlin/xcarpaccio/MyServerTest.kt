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

/*
class MyServerTest : StringSpec() {
    val TEST_PORT = 8001

    val logger: Logger = mock(Logger::class.java)

    override fun isolationMode(): IsolationMode? {
        return IsolationMode.InstancePerTest;
    }

    override fun extensions(): List<SpecLevelExtension> {
        return listOf(
                object : SpecExtension {
                    override suspend fun intercept(spec: Spec, process: suspend () -> Unit) {
                        var server = embeddedServer(Netty, TEST_PORT = PORT, module = Application::myModule)
                                .start()
                        process()
                        server.stop()
//                        myServer.shutdown()
                    }
                }
        )
    }

    init {
        "MyServer.kt should respond 'pong' when 'ping' is received" {
            val response = sendSimpleRequest("/ping", "GET")

            response.statusCode shouldBe 200
            response.body shouldBe "pong"
        }

        "MyServer.kt should log feedback message received via POST" {
            sendJson("/feedback", "POST", """{"type":"ERROR", "content":"The field \"total\" in the response is missing."}""")
            then(logger).should().log("""ERROR: The field "total" in the response is missing.""")
        }

        "MyServer.kt should respond to order" {
            val response = sendJson("/order", "POST", """{"prices":[3.5], "quantities":[2], "country":"FR", "reduction":"STANDARD"}""")
            then(logger).should().log("POST /order {quantities=[2], country=FR, prices=[3.5], reduction=STANDARD}")
            then(logger).should().log("Order(prices=[3.5], quantities=[2], country=FR, reduction=STANDARD)")

            response.statusCode shouldBe 200
            response.body shouldBe ""
        }
    }

    private fun sendSimpleRequest(url: String, method: String, headers: HashMap<String, String> = hashMapOf()): HttpResponse {
        val request = createHttpRequestInstance(url, method)
        return executeRequest(headers, request)
    }

    private fun sendJson(url: String, method: String, json: String, headers: HashMap<String, String> = hashMapOf()): HttpResponse {
        val httpRequest = createHttpRequestInstance(url, method)

        if (httpRequest is HttpEntityEnclosingRequestBase) {
            httpRequest.setHeader("Content-Type", "application/json")
            httpRequest.entity = StringEntity(json)
        } else {
            throw Exception("Cannot send JSON with this HTTP method ($method).")
        }

        return executeRequest(headers, httpRequest)
    }

    private fun createHttpRequestInstance(url: String, method: String): HttpRequestBase {
        val fullUrl = "http://localhost:" + TEST_PORT + url

        return when (method) {
            "GET" -> {
                HttpGet(fullUrl)
            }
            "POST" -> {
                HttpPost(fullUrl)
            }
            else -> {
                throw Exception("Invalid method")
            }
        }
    }

    private fun executeRequest(headers: HashMap<String, String>, request: HttpRequestBase): HttpResponse {
        val httpClient = HttpClientBuilder.create().build()
        for ((key, value) in headers) {
            request.setHeader(key, value)
        }
        request.setHeader("Connection", "Close")

        val response = httpClient.execute(request)!!
        val body = if (response.entity != null) {
            EntityUtils.toString(response.entity)
        } else {
            null
        }
        val responseHeaders = response.allHeaders!!

        return HttpResponse(responseHeaders, body,
                response.statusLine?.statusCode!!,
                response.statusLine?.reasonPhrase ?: "")
    }

    data class HttpResponse(val headers: Array<Header>, val body: String?, val statusCode: Int, val statusDescription: String)
}
*/

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

}