package xcarpaccio;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.then;
import static org.mockito.Mockito.mock;

public class MyHttpServerTest extends AbstractHttpServerTest
{
    private static final int TEST_PORT = 8001;
    private static final String LOCALHOST = "http://localhost:" + TEST_PORT;

    private MyHttpServer server;
    private Logger logger;

    @Before
    public void startServer() throws Exception {
        logger = mock(Logger.class);
        server = new MyHttpServer(TEST_PORT, logger);
        server.start();
    }

    @After
    public  void stopServer() {
        server.shutdown();
    }

    @Test
    public void should_respond_pong_when_receive_ping() throws Exception {
        assertThat(get(LOCALHOST + "/ping")).isEqualTo("pong");
    }

    @Test
    public void should_print_received_message_via_post() throws Exception {
        post(LOCALHOST + "/feedback", new Message("info", "test").json());

        then(logger).should().log("info: test");
    }
}
