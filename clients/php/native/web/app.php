<?php

declare(strict_types = 1);

include __DIR__.'/../vendor/autoload.php';

$actions = [
    '/' => function () {
        header('HTTP/1.1 204 No Content');
        header('Content-Type: text/plain; charset=utf8');
    },
];

$route = in_array($_SERVER['REQUEST_URI'], ['/', '/ping', ]) ? $_SERVER['REQUEST_URI'] : '/';

$actions[$route]();
