<?php

class FeedbackMessage
{
    private $_type;
    private $_content;

    public function __construct(string $type, string $content)
    {
        $this->_type = $type;
        $this->_content = $content;
    }

    public function GetMessage(): string
    {
        return $this->_content;
    }

    public function GetType()
    {
        return $this->_type;
    }
}