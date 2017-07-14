package xcarpaccio

import io.kotlintest.Spec
import io.kotlintest.matchers.shouldBe
import io.kotlintest.mock.mock
import io.kotlintest.specs.StringSpec
import org.apache.http.Header
import org.apache.http.client.methods.HttpEntityEnclosingRequestBase
import org.apache.http.client.methods.HttpGet
import org.apache.http.client.methods.HttpPost
import org.apache.http.client.methods.HttpRequestBase
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.HttpClientBuilder
import org.apache.http.util.EntityUtils
import org.mockito.BDDMockito.then
import java.util.*

class MyServerTest : StringSpec() {
    val TEST_PORT = 8001

    val logger: Logger = mock()
    val myServer: MyServer = MyServer(TEST_PORT, logger)

    override val oneInstancePerTest = false

    override fun interceptSpec(context: Spec, spec: () -> Unit) {
        myServer.start(false)
        spec()
        myServer.shutdown()
    }

    init {
        "MyServer should respond 'pong' when 'ping' is received" {
            val response = sendSimpleRequest("/ping", "GET")

            response.statusCode shouldBe 200
            response.body shouldBe "pong"
        }

        "MyServer should log feedback message received via POST" {
            sendJson("/feedback", "POST", """{"type":"ERROR", "content":"The field \"total\" in the response is missing."}""")
            then(logger).should().log("""ERROR: The field "total" in the response is missing.""")
        }

        "MyServer should respond to order" {
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
            "GET" -> { HttpGet(fullUrl) }
            "POST" -> { HttpPost(fullUrl) }
            else  -> { throw Exception("Invalid method") }
        }
    }

    private fun executeRequest(headers: HashMap<String, String>, request: HttpRequestBase): HttpResponse {
        val httpClient = HttpClientBuilder.create().build()
        for ((key, value) in headers) {
            request.setHeader(key, value)
        }
        request.setHeader("Connection", "Close")

        val response = httpClient.execute(request)!!
        val body = if (response.entity != null) { EntityUtils.toString(response.entity) } else { null }
        val responseHeaders = response.allHeaders!!

        return HttpResponse(responseHeaders, body,
                response.statusLine?.statusCode!!,
                response.statusLine?.reasonPhrase ?: "")
    }

    data class HttpResponse(val headers: Array<Header>, val body: String?, val statusCode: Int, val statusDescription: String)
}
