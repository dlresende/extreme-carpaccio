<?php

declare(strict_types=1);

namespace ExtremeCarpaccio\Tests;

use ExtremeCarpaccio\Router;
use PHPUnit\Framework\Attributes\CoversClass;
use PHPUnit\Framework\TestCase;

#[CoversClass(Router::class)]
final class RouterTest extends TestCase
{
    public function testPostPingRespondsWithPong(): void
    {
        [$status, , $body] = (new Router())->handle('POST', '/ping');

        $this->assertSame(200, $status);
        $this->assertSame('pong', $body);
    }
}
