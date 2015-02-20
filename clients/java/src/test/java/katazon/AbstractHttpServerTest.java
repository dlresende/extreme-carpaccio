package katazon;

import org.junit.AfterClass;
import org.junit.BeforeClass;

import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Scanner;

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

    private String stringify(InputStream is) {
        Scanner scanner = new Scanner(is).useDelimiter("\\A");
        return scanner.hasNext() ? scanner.next() : "";
    }

    @AfterClass
    public static void stopServer() {
        server.shutdown();
    }
}
