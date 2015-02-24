package extremecarpaccio;

import org.junit.Rule;
import org.junit.Test;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;

import static extremecarpaccio.Strings.stringify;
import static org.fest.assertions.Assertions.assertThat;

public class HttpServerTest {

    @Rule
    public HttpServerResource resource = new HttpServerResource(1338);

    @Test
    public void usecase() throws IOException {
        assertThat(get("/hello")).isEqualTo("Hello World");
    }

    protected String get(String path) throws IOException {
        return get(path, null);
    }

    protected String get(String path, String query) throws IOException {
        HttpURLConnection connection = null;
        try {
            String queryPart = (query != null ? ("?" + query) : "");
            URL url = new URL(resource.baseURL() + path + queryPart);
            connection = (HttpURLConnection) url.openConnection();
            return stringify(connection.getInputStream(), "utf-8");
        } finally {
            if (connection != null)
                connection.disconnect();
        }
    }

}