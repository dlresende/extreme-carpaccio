package extremecarpaccio;

import org.junit.Rule;
import org.junit.Test;

import java.io.DataOutputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;

import static extremecarpaccio.Strings.stringify;
import static org.assertj.core.api.Assertions.assertThat;

public class HttpServerTest {

    private static final String UTF_8 = "utf-8";

    @Rule
    public HttpServerResource resource = new HttpServerResource(1338);

    @Test
    public void should_respond_pong_on_get_ping() throws IOException {
        assertThat(get("/ping")).isEqualTo("pong");
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
            return stringify(connection.getInputStream(), UTF_8);
        } finally {
            if (connection != null)
                connection.disconnect();
        }
    }

    protected String post(String path, String body) throws IOException {
        URL url = new URL(path);
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        connection.setDoOutput(true);
        connection.setDoInput(true);
        connection.setInstanceFollowRedirects(false);
        connection.setRequestMethod("POST");
        connection.setRequestProperty("Content-Type", "application/x-www-form-urlencoded");
        connection.setRequestProperty("charset", UTF_8);
        connection.setRequestProperty("Content-Length", Integer.toString(body.length()));
        connection.setUseCaches(false);
        try( DataOutputStream wr = new DataOutputStream( connection.getOutputStream())) {
            wr.write( body.getBytes(UTF_8) );
            return stringify(connection.getInputStream(), UTF_8);
        } finally {
            connection.disconnect();
        }
    }

}