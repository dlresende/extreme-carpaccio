package extremecarpaccio;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

import static org.junit.jupiter.api.Assertions.assertEquals;

class HttpServerTest {
    private static final int TEST_PORT = 1338;

    @AfterEach
    void stopServer() {
        HttpServer.stopServer();
    }

    @Test
    void postPingReturnsPong() throws Exception {
        HttpServer.start(TEST_PORT);
        HttpRequest request = HttpRequest.newBuilder(URI.create("http://localhost:" + TEST_PORT + "/ping"))
                .POST(HttpRequest.BodyPublishers.noBody())
                .build();

        HttpResponse<String> response = HttpClient.newHttpClient()
                .send(request, HttpResponse.BodyHandlers.ofString());

        assertEquals(200, response.statusCode());
        assertEquals("pong", response.body());
    }
}
