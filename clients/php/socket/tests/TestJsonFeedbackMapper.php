<?php


use PHPUnit\Framework\TestCase;

class TestJsonFeedbackMapper extends TestCase
{
    /**
     * @test
     */
    public function should_return_mapped_feedback_when_input_is_valid_json()
    {
        $expectedFeedbackMessage = new FeedbackMessage("ERROR", "The error message");

        $json = "{\"type\":\"ERROR\",\"content\":\"The error message\"}";

        $feedbackMapper = new JsonFeedbackMapper();
        $feedbackMessage = $feedbackMapper->setValues($json);

        $this->assertEquals($expectedFeedbackMessage, $feedbackMessage);
    }
}
