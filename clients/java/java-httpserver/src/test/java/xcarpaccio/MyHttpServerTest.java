package xcarpaccio;

import org.junit.jupiter.api.Test;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

import static org.junit.jupiter.api.Assertions.assertEquals;

class MyHttpServerTest {
    @Test
    void postPingReturnsPong() throws Exception {
        try (MyHttpServer server = new MyHttpServer(0)) {
            server.start();
            HttpRequest request = HttpRequest.newBuilder(URI.create("http://localhost:" + server.port() + "/ping"))
                    .POST(HttpRequest.BodyPublishers.noBody())
                    .build();

            HttpResponse<String> response = HttpClient.newHttpClient()
                    .send(request, HttpResponse.BodyHandlers.ofString());

            assertEquals(200, response.statusCode());
            assertEquals("pong", response.body());
        }
    }
}
