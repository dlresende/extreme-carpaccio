<?php


class Response implements ResponseInterface
{

    public function Post($result, $spawn)
    {
        $answer = $this->formatJsonAnswer($result);
        socket_write($spawn, $answer, strlen($answer));
    }

    private function formatJsonAnswer($result)
    {

        $answer = json_encode($result);
        $result = "HTTP/1.0 200 OK\n";
        $result .= "Content-Type: application/json\n";
        $result .= "Content-Length: " . strlen($answer) . "\n";
        $result .= "\r\n";
        $result .= $answer;

        return $result;
    }
}