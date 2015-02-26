package xcarpaccio;

import org.junit.AfterClass;
import org.junit.BeforeClass;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;

import static xcarpaccio.StringUtils.stringify;

public abstract class AbstractHttpServerTest
{
    private static final int TEST_PORT = 8001;
    private static final String LOCALHOST = "http://localhost:" + TEST_PORT;
    private static MyHttpServer server = new MyHttpServer(TEST_PORT);

    @BeforeClass
    public static void startServer() throws Exception {
        server.start();
    }

    protected String get(String path) throws IOException {
        return get(path, "");
    }

    protected String get(String path, String query) throws IOException {
        URL localhost = new URL(LOCALHOST + path + "?" + query);
        HttpURLConnection connection = (HttpURLConnection) localhost.openConnection();
        connection.disconnect();
        return stringify(connection.getInputStream());
    }

    @AfterClass
    public static void stopServer() {
        server.shutdown();
    }
}
