<?php

namespace Tests\Functionals\ExtremeCarpaccio;


class ApiContext extends FeatureContext
{
    /**
     * @var string
     */
    protected $baseUrl;

    /**
     * ApiContext constructor.
     *
     * @param string $baseUrl
     */
    public function __construct($baseUrl)
    {
        $this->baseUrl = $baseUrl;
    }
}
