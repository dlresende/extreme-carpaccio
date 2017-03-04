Feature: Registration validation
    In order to play
    As a user
    I must confirm my registration

Scenario: Registration confirmation
    Given I am a pre-registered user
    When I receive the registration validation
    Then I must validate my registration
