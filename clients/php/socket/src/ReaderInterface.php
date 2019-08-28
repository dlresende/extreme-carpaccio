<?php

interface ReaderInterface
{
    public function GetResource();
    public function Listen($port);
    public function Read();
    public function GetMessage();
    public function Close();
}