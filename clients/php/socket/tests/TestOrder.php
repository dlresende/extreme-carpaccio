<?php


use PHPUnit\Framework\TestCase;

class TestOrder extends TestCase
{
    /**
     * @test
     */
    public function should_()
    {
        $order = new Order();
        $actual = new Result(30);

        $expected = new Result(30);
        $this->assertEquals($expected, $actual);
    }
}
