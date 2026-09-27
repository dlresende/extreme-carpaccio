<?php

declare(strict_types=1);

namespace ExtremeCarpaccio;

final class Router
{
    /**
     * @return array{status: int, contentType: string, body: string}
     */
    public function handle(string $method, string $path): array
    {
        if ($method === 'POST' && $path === '/ping') {
            return [200, 'text/plain; charset=utf-8', 'pong'];
        }

        return [404, 'text/plain; charset=utf-8', 'Not Found'];
    }
}
