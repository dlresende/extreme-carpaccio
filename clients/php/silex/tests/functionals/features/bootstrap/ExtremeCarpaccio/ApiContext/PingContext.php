<?php

namespace Tests\Functionals\ExtremeCarpaccio\ApiContext;

use Tests\Functionals\ExtremeCarpaccio\ApiContext as Base;

/**
 * Class PingContext
 */
class PingContext extends Base
{
    /**
     * @Given I am a pre-registered user
     */
    public function iAmAPreRegisteredUser()
    {

    }

    /**
     * @When I receive the registration validation
     */
    public function iReceiveTheRegistrationValidation()
    {
        $this->visitPath('/ping');
    }

    /**
     * @Then I must validate my registration
     */
    public function iMustValidateMyRegistration()
    {
        $this->assertResponseContains('pong');
    }
}
