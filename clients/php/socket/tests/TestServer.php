<?php

use PHPUnit\Framework\TestCase;

class TestServer extends TestCase
{
    /**
     * @test
     */
    public function should_post_result_when_order_received()
    {
        $result = new Result(30);

        $response = $this->createMock(ResponseInterface::class);
        $reader = $this->createMock(ReaderInterface::class);
        $console = $this->createMock(OutputInterface::class);
        $orderMapper = $this->createMock(OrderMapperInterface::class);
        $order = $this->createMock(Order::class);
        $feedbackMapper = $this->createMock(FeedbackMapperInterface::class);

        $order->method('Calculate')->willReturn($result);
        $orderMapper->method('SetValues')->willReturn($order);

        $reader->method('GetType')->willReturn('Order');

        $response->expects($this->exactly(1))
            ->method('Post')
            ->with(
                $result, $this->anything()
            );

        $server = new Server($reader, $response, $console, $orderMapper, $feedbackMapper);
        $server->Start();
    }

    /**
     * @test
     */
    public function should_print_message_when_feedback_received()
    {
        $expected = "ERROR : Feedback message";

        $response = $this->createMock(ResponseInterface::class);
        $reader = $this->createMock(ReaderInterface::class);
        $console = $this->createMock(OutputInterface::class);
        $orderMapper = $this->createMock(OrderMapperInterface::class);

        $feedbackMapper = $this->createMock(FeedbackMapperInterface::class);
        $feedbackMessage = $this->createMock(FeedbackMessage::class);

        $feedbackMessage->method('GetMessage')->willReturn("Feedback message");
        $feedbackMessage->method('GetType')->willReturn("ERROR");

        $feedbackMapper->method('setValues')->willReturn($feedbackMessage);

        $reader->method('GetType')->willReturn('Feedback');

        $server = new Server($reader, $response, $console, $orderMapper, $feedbackMapper);

        $console->expects($this->exactly(1))
            ->method('Print')
            ->with(
                $this->equalTo($expected)
            );

        $server->Start();
    }
}
