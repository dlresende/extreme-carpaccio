<?php

declare(strict_types=1);

use ExtremeCarpaccio\Router;

require __DIR__ . '/../vendor/autoload.php';

$method = $_SERVER['REQUEST_METHOD'] ?? 'GET';
$path = parse_url($_SERVER['REQUEST_URI'] ?? '/', PHP_URL_PATH) ?: '/';

[$status, $contentType, $body] = (new Router())->handle($method, $path);

http_response_code($status);
header('Content-Type: ' . $contentType);

echo $body;
