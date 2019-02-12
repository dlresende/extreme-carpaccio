package xcarpaccio;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.test.context.junit4.SpringRunner;

import static org.junit.Assert.assertEquals;

/**
 * Testing web API, starting the server
 */
@RunWith(SpringRunner.class)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
public class WebControllerServerTest {

    @LocalServerPort
    private int port;

    @Autowired
    private TestRestTemplate restTemplate;

    @Test
    public void orderWithEmptyPriceListShouldReturn0() {
        TestRestTemplate testRestTemplate = new TestRestTemplate();
        Order order = new Order();
        order.prices = new Double[] {};
        ResponseEntity<Amount> response = testRestTemplate.
                postForEntity("http://localhost:" + this.port + "/order", order, Amount.class);
        assertEquals(HttpStatus.OK, response.getStatusCode());
        assertEquals(new Double(0.0), response.getBody().total);
    }

    @Test
    public void aNominalTest() {
        TestRestTemplate testRestTemplate = new TestRestTemplate();
        Order order = new Order();
        order.prices = new Double[] {0.0};
        ResponseEntity<Amount> response = testRestTemplate.
                postForEntity("http://localhost:" + this.port + "/order", order, Amount.class);
        // Throws a 404 for now, to avoid penalty
        assertEquals(HttpStatus.NOT_FOUND, response.getStatusCode());
    }
}
