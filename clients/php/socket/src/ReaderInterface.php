<?php

interface ReaderInterface
{
    public function GetType();
    public function Listen($port);
    public function Read();
    public function GetMessage();
    public function Close();
}