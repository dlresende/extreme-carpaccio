<?php


use PHPUnit\Framework\TestCase;

class TestReader extends TestCase
{
    /**
     * @test
     */
    public function should_type_order_when_message_received()
    {
        $body = "POST /order HTTP/1.1
Content-Type: application/json
Accept: application/json
Content-Length: 136
Host: 10.66.82.115:9003
Connection: close

{\"prices\":[69.61,48.41,65.77,84.48,60.35,96.21,84.8,47.3,16.37],\"quantities\":[5,6,8,9,10,6,3,2,2],\"country\":\"SK\",\"reduction\":\"STANDARD\"}\"";

        $reader = new Reader();
        $commandType = $reader->GetResourceFrom($body);

        $this->assertEquals("Order", $commandType);

    }

    /**
     * @test
     */
    public function should_type_feedback_when_message_received()
    {
        $body = "POST /feedback HTTP/1.1
Content-Type: application/json
Accept: application/json
Content-Length: 136
Host: 10.66.82.115:9003
Connection: close

{\"prices\":[69.61,48.41,65.77,84.48,60.35,96.21,84.8,47.3,16.37],\"quantities\":[5,6,8,9,10,6,3,2,2],\"country\":\"SK\",\"reduction\":\"STANDARD\"}\"";

        $reader = new Reader();
        $commandType = $reader->GetResourceFrom($body);

        $this->assertEquals("Feedback", $commandType);

    }

    /**
     * @test
     */
    public function should_return_message_when_order_received()
    {
        $body = "POST /order HTTP/1.1
Content-Type: application/json
Accept: application/json
Content-Length: 136
Host: 10.66.82.115:9003
Connection: close\r\n\r\n{\"prices\":[69.61,48.41,65.77,84.48,60.35,96.21,84.8,47.3,16.37],\"quantities\":[5,6,8,9,10,6,3,2,2],\"country\":\"SK\",\"reduction\":\"STANDARD\"}\"";

        $expected = "{\"prices\":[69.61,48.41,65.77,84.48,60.35,96.21,84.8,47.3,16.37],\"quantities\":[5,6,8,9,10,6,3,2,2],\"country\":\"SK\",\"reduction\":\"STANDARD\"}\"";

        $reader = new Reader();
        $message = $reader->GetMessageFrom($body);

        $this->assertEquals($expected, $message);

    }
}
