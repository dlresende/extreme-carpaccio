<?php


use PHPUnit\Framework\TestCase;

class TestJsonOrderMapper extends TestCase
{
    /**
     * @test
     */
    public function should_return_mapped_order_when_input_is_valid_json()
    {
        $expectedOrder = new Order();
        $expectedOrder->prices = array(10.4, 5.6, 70.7);
        $expectedOrder->quantities = array(3, 5, 8);
        $expectedOrder->country = "FR";
        $expectedOrder->reduction = "STANDARD";

        $json = "{\"prices\":[10.4, 5.6, 70.7],\"quantities\":[3, 5, 8],\"country\":\"FR\",\"reduction\":\"STANDARD\"}";

        $orderMapper = new JsonOrderMapper();
        $order = $orderMapper->setValues($json);

        $this->assertEquals($expectedOrder, $order);
    }
}
