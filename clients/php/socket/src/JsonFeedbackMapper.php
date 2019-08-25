<?php

class JsonFeedbackMapper implements FeedbackMapperInterface
{

    /**
     * JsonFeedbackMapper constructor.
     */
    public function __construct()
    {
    }

    public function setValues($input): FeedbackMessage
    {
        $json = json_decode($input, true);

        $feedbackMessage = new FeedbackMessage($json['type'], $json['content']);

        return $feedbackMessage;
    }
}