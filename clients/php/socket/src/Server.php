<?php


class Server
{
    private $_reader;
    private $_response;
    private $_output;
    private $_feedbackMapper;

    public function __construct
    (
        ReaderInterface $reader, ResponseInterface $response, OutputInterface $output, FeedbackMapperInterface $feedbackMapper
    )
    {
        $this->_reader = $reader;
        $this->_response = $response;
        $this->_output = $output;
        $this->_feedbackMapper = $feedbackMapper;
    }

    public function Start()
    {
        $this->_reader->Listen(5000);
        do {
            $readerSpawn = $this->_reader->Read();
            $resource = $this->_reader->GetResource();
            if($resource == "Ping"){
                $this->_output->Print($resource);
                $this->_response->Post("pong", $readerSpawn);
            }
            if ($resource == "Order") {
                $this->_output->Print($resource . " : " . $this->_reader->GetMessage());

                //$result = new Result(30); //TODO compute correct result or you'll get a penalty
                $result = null;

                $this->_response->Post($result, $readerSpawn);
            }
            if ($resource == "Feedback"){
                $feedbackMessage = $this->_feedbackMapper->setValues($this->_reader->GetMessage());
                $this->_output->Print($feedbackMessage->GetType() . " : " . $feedbackMessage->GetMessage());
            }
        }
        while($readerSpawn);
        $this->_reader->Close();
    }
}