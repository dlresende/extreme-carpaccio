package xcarpaccio;

import org.junit.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Testing business methods that don't require Spring at all, using assertj
 */
public class WebControllerBusinessTest {

    @Test
    public void defaultComputedAmountShouldBeZero() {
        assertThat((new WebController()).computeAmount(new Order())).isEqualTo(0.0);
    }
}
