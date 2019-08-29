<?php


include "Result.php";
include "Order.php";
include "ReaderInterface.php";
include "ResponseInterface.php";
include "FeedbackMapperInterface.php";
include "FeedbackMessage.php";
include "JsonFeedbackMapper.php";

include "Reader.php";
include "Response.php";

include "OutputInterface.php";
include "Console.php";
include "Server.php";

$reader = new Reader();
$response = new Response();
$output = new Console();
$feedbackMapper = new JsonFeedbackMapper();

$server = new Server($reader, $response, $output, $feedbackMapper);
$server->Start();