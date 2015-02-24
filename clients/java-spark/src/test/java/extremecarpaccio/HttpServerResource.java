package extremecarpaccio;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.ConnectException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.concurrent.TimeoutException;

public class HttpServerResource extends org.junit.rules.ExternalResource {

    private static Logger logger = LoggerFactory.getLogger(HttpServerResource.class);

    private final int port;

    public HttpServerResource(int port) {
        this.port = port;
    }

    @Override
    protected void before() throws Throwable {
        HttpServer.start(port);
        waitForServerToBeStarted(port);
    }

    private static void waitForServerToBeStarted(int port) throws IOException, TimeoutException, InterruptedException {

        // watchout ugly code!
        int timeoutInMillis = 10 * 1000;
        int waitBetweenAttemptInMillis = 100;
        int maxAttempt = timeoutInMillis / waitBetweenAttemptInMillis;

        int attempt = 1;
        while (attempt < maxAttempt) {
            HttpURLConnection connection = null;
            try {
                URL url = new URL("http://127.0.0.1:" + port);
                connection = (HttpURLConnection) url.openConnection();
                connection.connect();
                return;
            } catch (ConnectException e) {
                logger.debug("Server not yet reachable ({}/{})", attempt, maxAttempt);
                attempt++;
                Thread.sleep(waitBetweenAttemptInMillis);
            } finally {
                if (connection != null)
                    connection.disconnect();
            }
        }
        throw new TimeoutException("Server not started in time");
    }

    @Override
    protected void after() {
        HttpServer.stop();
    }

    public String baseURL() {
        return "http://127.0.0.1:" + port;
    }
}
