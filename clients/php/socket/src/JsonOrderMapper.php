<?php

class JsonOrderMapper implements OrderMapperInterface
{

    public function setValues($input) : Order
    {
        $json = json_decode($input, true);

        $order = new Order();
        $order->prices = $json['prices'];
        $order->quantities = $json['quantities'];
        $order->country = $json['country'];
        $order->reduction = $json['reduction'];

        return $order;
    }
}