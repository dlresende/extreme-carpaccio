package xcarpaccio;

import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class WebController {
    @PostMapping("/ping")
    public String ping() {
        return "pong";
    }
}
