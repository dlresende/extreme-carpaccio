<?php

namespace Tests\Functionals\ExtremeCarpaccio\ApiContext;

use Behat\Gherkin\Node\TableNode;
use Tests\Functionals\ExtremeCarpaccio\ApiContext as Base;

/**
 * Class OrderContext
 */
class OrderContext extends Base
{
    /**
     * @var array
     */
    private $prices;

    /**
     * @var array
     */
    private $quantities;

    /**
     * @var string
     */
    private $country;

    /**
     * @var string
     */
    private $reduction;


    public function __construct($baseUrl)
    {
        parent::__construct($baseUrl);

        $this->quantities = [];
        $this->prices = [];
        $this->reduction = 'STANDARD';
    }

    /**
     * @When I receive a bad request
     */
    public function iReceiveABadRequest()
    {
        $this->visitPath('/order');
    }

    /**
     * @Then I handle the bad request
     */
    public function iHandleTheBadRequest()
    {
        $this->assertResponseStatus(405);
    }

    /**
     * @Given I have the following order
     */
    public function iHaveTheFollowingOrder(TableNode $table)
    {
        $this->prices = $this->quantities = [];

        foreach ($table as $row ) {
            $this->prices[] = (float) $row['price'];
            $this->quantities[] = (int) $row['quantity'];
        }
    }

    /**
     * @Given the country is :country
     */
    public function theCountryIs($country)
    {
        $this->country = $country;
    }

    /**
     * @Given the reduction is :reduction
     */
    public function theReductionIs($reduction)
    {
        $this->reduction = $reduction;
    }

    /**
     * @When I validate
     */
    public function iValidate()
    {
        $post = [
            'prices' => $this->prices,
            'quantities' => $this->quantities,
            'country' => $this->country,
            'reduction' => $this->reduction,
        ];
        $this->getSession()->getDriver()->getClient()->request('POST', $this->baseUrl.'/order', $post);
        $this->assertResponseStatus(200);
   }

    /**
     * @Then The total must be :total €
     */
    public function theTotalMustBeEu($total)
    {
        $content = json_decode($this->getSession()->getDriver()->getContent(), true);
        assert($content['total'] == $total);
    }
}
