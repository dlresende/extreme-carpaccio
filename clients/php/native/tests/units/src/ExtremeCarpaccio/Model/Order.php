<?php

namespace Tests\Units\ExtremeCarpaccio\Model;

use Tests\Units\Test;

class Order extends Test
{
    function testConstruct()
    {
        $this
            ->given(
                $country = uniqid('country'),
                $product = new \mock\ExtremeCarpaccio\Model\OrderProductInterface,
                $this->calling($product)->getUnitPrice = mt_rand(),
                $this->calling($product)->getQuantity = mt_rand(),
                $products = [
                    $product
                ],
                $reductionType = uniqid('reductionType')
            )
            ->if(
                $this->newTestedInstance($country, $products, $reductionType)
            )
            ->then
                ->object($this->testedInstance)->isInstanceOf(\ExtremeCarpaccio\Model\OrderInterface::class)
                ->string($this->testedInstance->getCountry())->isEqualTo($country)
                ->array($this->testedInstance->getProducts())->isIdenticalTo($products)
                ->string($this->testedInstance->getReductionType())->isEqualTo($reductionType)
        ;
    }
}
