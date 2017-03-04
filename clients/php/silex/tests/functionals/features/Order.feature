Feature: Order management
    In order to calculate the bill
    I must compute the amount of the received orders

    Scenario: Order without reduction
        Given I have the following order
            | price | quantity |
            | 15.99 | 1        |
        And the country is "ES"
        And the reduction is "STANDARD"
        When I validate
        Then The total must be 19.03 €
