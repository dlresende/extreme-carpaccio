<?php

class Order
{
    public $prices = array();
    public $quantities = array();
    public $country;
    public $reduction;

    public function __construct()
    {

    }

    public function Calculate() : Result
    {
        return new Result(30.00);
    }


}