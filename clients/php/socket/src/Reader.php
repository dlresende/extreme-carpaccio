<?php

class Reader implements ReaderInterface
{

    /**
     * @var resource
     */
    private $_socket;
    /**
     * @var string
     */
    private $_message;
    /**
     * @var string
     */
    private $_type;

    public function GetType()
    {
        return $this->_type;
    }

    public function GetMessage()
    {
        return $this->_message;
    }

    public function Listen($port)
    {
        $host = "0.0.0.0";
        $this->_socket = socket_create(AF_INET, SOCK_STREAM, SOL_TCP);
        socket_bind($this->_socket, $host, $port);
        socket_listen($this->_socket, SOMAXCONN);
    }

    public function Read()
    {
        try {
            $spawn = socket_accept($this->_socket);
            $body = socket_read($spawn, 1048576);
            $this->_message = $this->GetMessageFrom($body);
            $this->_type = $this->GetTypeFrom($body);
            return $spawn;
        }
        catch(Exception $e)
        {
            throw $e;
        }
    }

    public function Close()
    {
        socket_close($this->_socket);
    }

    public function GetMessageFrom(string $body)
    {
        $parts = explode("\r\n\r\n", $body);

        if ($parts)
        {
            $message = $parts[1];
        }

        return $message;
    }

    public function GetTypeFrom(string $body)
    {
        $parts = explode("\r\n\r\n", $body);
        $type = "";
        if ($parts)
        {
            $headers = array_shift($parts);
            $matches = preg_split('/ /', $headers);
            $type = str_replace("/", "", $matches[1]);
        }

        return ucfirst($type);
    }
}

function getUserIpAddr(){
    if(!empty($_SERVER['HTTP_CLIENT_IP'])){
        //ip from share internet
        $ip = $_SERVER['HTTP_CLIENT_IP'];
    }elseif(!empty($_SERVER['HTTP_X_FORWARDED_FOR'])){
        //ip pass from proxy
        $ip = $_SERVER['HTTP_X_FORWARDED_FOR'];
    }else{
        $ip = $_SERVER['REMOTE_ADDR'];
    }
    return $ip;
}