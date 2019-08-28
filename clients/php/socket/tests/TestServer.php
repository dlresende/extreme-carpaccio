<?php

use PHPUnit\Framework\TestCase;

class TestServer extends TestCase
{
    /**
     * @test
     */
    public function should_return_pong_when_ping_received()
    {
        $expected = "Ping";
        $response = $this->createMock(ResponseInterface::class);
        $reader = $this->createMock(ReaderInterface::class);
        $console = $this->createMock(OutputInterface::class);

        $feedbackMapper = $this->createMock(FeedbackMapperInterface::class);

        $reader->method('GetResource')->willReturn('Ping');

        $server = new Server($reader, $response, $console, $feedbackMapper);

        $console->expects($this->exactly(1))
            ->method('Print')
            ->with(
                $this->equalTo($expected)
            );

        $response->expects($this->exactly(1))
            ->method('Post')
            ->with(
                'pong',
                $this->anything()
            );

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

        $feedbackMapper = $this->createMock(FeedbackMapperInterface::class);
        $feedbackMessage = $this->createMock(FeedbackMessage::class);

        $feedbackMessage->method('GetMessage')->willReturn("Feedback message");
        $feedbackMessage->method('GetType')->willReturn("ERROR");

        $feedbackMapper->method('setValues')->willReturn($feedbackMessage);

        $reader->method('GetResource')->willReturn('Feedback');

        $server = new Server($reader, $response, $console, $feedbackMapper);

        $console->expects($this->exactly(1))
            ->method('Print')
            ->with(
                $this->equalTo($expected)
            );

        $server->Start();
    }
}
