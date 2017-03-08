<?php

namespace Tests\Units\ExtremeCarpaccio\Model;

use Tests\Units\Test;

class OrderProduct extends Test
{
    function testConstructor()
    {
        $this
            ->given(
                $unitPrice = mt_rand() / mt_rand(),
                $quantity = mt_rand()
            )
            ->if(
                $this->newTestedInstance($unitPrice, $quantity)
            )
            ->then
                ->integer($this->testedInstance->getQuantity())->isEqualTo($quantity)
                ->float($this->testedInstance->getUnitPrice())->isEqualTo($unitPrice)
        ;
    }
}
