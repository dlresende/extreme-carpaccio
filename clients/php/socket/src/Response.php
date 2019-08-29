<?php


class Response implements ResponseInterface
{

    public function Post($result, $spawn)
    {
        $answer = $this->formatAnswer($result);
        socket_write($spawn, $answer, strlen($answer));
    }

    private function formatAnswer($result)
    {
        $answer = "";
        if($result != null) {
            $answer = json_encode($result);
        }
        $result = $this->GetHeader($answer);
        $result .= $answer;

        return $result;
    }

    private function GetHeader(string $answer): string
    {
        $result = "HTTP/1.0 200 OK\n";
        $result .= "Content-Type: application/json\n";
        $result .= "Content-Length: " . strlen($answer) . "\n";
        $result .= "\r\n";
        return $result;
    }
}