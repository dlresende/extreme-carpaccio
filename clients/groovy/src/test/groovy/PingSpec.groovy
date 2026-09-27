import spock.lang.Specification

import java.net.URI
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse

class PingSpec extends Specification {

    def "POST /ping responds with pong"() {
        given:
        def server = new Server(13003)
        server.start()

        when:
        def response = HttpClient.newHttpClient().send(
                HttpRequest.newBuilder(URI.create("http://localhost:${server.port()}/ping"))
                        .POST(HttpRequest.BodyPublishers.noBody())
                        .build(),
                HttpResponse.BodyHandlers.ofString())

        then:
        response.statusCode() == 200
        response.body() == 'pong'

        cleanup:
        server?.close()
    }
}
