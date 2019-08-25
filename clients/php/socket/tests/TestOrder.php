<?php


use PHPUnit\Framework\TestCase;

class TestOrder extends TestCase
{
    /**
     * @test
     */
    public function should_return_30()
    {
        $order = new Order();
        $result = $order->Calculate();

        $expected = new Result(30);
        $this->assertEquals($expected, $result);
    }
}
