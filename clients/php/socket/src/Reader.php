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
    private $_resource;

    public function GetResource()
    {
        return $this->_resource;
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

        $spawn = socket_accept($this->_socket);
        $body = socket_read($spawn, 1048576);
        $this->_message = $this->GetMessageFrom($body);
        $this->_resource = $this->GetResourceFrom($body);
        return $spawn;

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

    public function GetResourceFrom(string $body)
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