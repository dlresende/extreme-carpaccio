package katazon;

import org.junit.Test;

import static org.fest.assertions.Assertions.assertThat;

public class MyHttpServerTest extends AbstractHttpServerTest
{
    @Test
    public void should_respond_pong_when_receive_ping() throws Exception {
        assertThat(get("/ping")).isEqualTo("pong");
    }
}
