<?php

class Console implements OutputInterface
{

    public function Print(string $string)
    {
        echo $string."\n\r";
    }
}