package xcarpaccio;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;

/**
 * Testing web API without the cost of starting the server but yet all the layers below
 * and for this using MockMvc
 */
@RunWith(SpringRunner.class)
@SpringBootTest
@AutoConfigureMockMvc
public class WebControllerMockedServerTest {

    @Autowired
    private MockMvc mockMvc;

    @Test
    public void orderWithEmptyPriceListShouldReturn0() throws Exception {
        this.mockMvc.perform(
                post("/order/")
                        .content("{\"prices\":[]}")
                        .contentType(MediaType.APPLICATION_JSON_UTF8))
                .andExpect(status().isOk())
                .andExpect(content().json("{\"total\":0}"))
        ;
    }

    @Test
    public void aNominalTest() throws Exception {
        this.mockMvc.perform(
                post("/order/")
                        .content("{\"prices\":[1]}")
                        .contentType(MediaType.APPLICATION_JSON_UTF8))
                // Throws a 404 for now, to avoid penalty
                .andExpect(status().isNotFound());
    }
}