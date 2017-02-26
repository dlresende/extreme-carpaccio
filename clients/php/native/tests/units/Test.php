<?php

namespace Tests\Units;

/**
 * Class Test
 */
class Test extends \atoum
{
    public function beforeTestMethod($method)
    {
        $this->mockGenerator->allIsInterface();
    }
}
