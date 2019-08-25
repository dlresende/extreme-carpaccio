<?php


class Server
{
    /**
     * @var \PHPUnit\Framework\MockObject\MockObject|ReaderInterface
     */
    private $_reader;
    /**
     * @var ResponseInterface
     */
    private $_response;
    /**
     * @var OutputInterface
     */
    private $_output;
    /**
     * @var OrderMapperInterface
     */
    private $_orderMapper;
    /**
     * @var FeedbackMapperInterface
     */
    private $_feedbackMapper;

    /**
     * XCServer constructor.
     * @param \PHPUnit\Framework\MockObject\MockObject $reader
     * @param $response
     */
    public function __construct
    (
        ReaderInterface $reader,
        ResponseInterface $response,
        OutputInterface $output,
        OrderMapperInterface $orderMapper,
        FeedbackMapperInterface $feedbackMapper
    )
    {
        $this->_reader = $reader;
        $this->_response = $response;
        $this->_output = $output;
        $this->_orderMapper = $orderMapper;
        $this->_feedbackMapper = $feedbackMapper;
    }

    public function Start()
    {
        $this->_reader->Listen(9004);
        do {
            $readerSpawn = $this->_reader->Read();
            $readerType = $this->_reader->GetType();

            if ($readerType == "Order") {
                $this->_output->Print($readerType . " : " . $this->_reader->GetMessage());
                $order = $this->_orderMapper->setValues($this->_reader->GetMessage());
                $result = $order->Calculate();
                $this->_response->Post($result, $readerSpawn);
            }
            if ($readerType == "Feedback"){
                $feedbackMessage = $this->_feedbackMapper->setValues($this->_reader->GetMessage());
                $this->_output->Print($feedbackMessage->GetType() . " : " . $feedbackMessage->GetMessage());
            }
        }
        while($readerSpawn);
        $this->_reader->Close();
    }
}