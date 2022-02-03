# TO DO LIST

## TODO
- [X] Write a test sending an http request
  - [X] Deal with all the cmake arcane...
- [X] Create a main exe that starts a server 
- [X] Manual test that it can display a bmp, test it in the browser
- [X] Update our test to check that client can get a file from the server
- [X] Deal with starting & stopping the server correctly in the test to avoid resource leaks
- [X] Implement the POST /order command
  - [X] check what the request and answers should look like from the js versions
  - [X] write a test to POST /order with a proper request
  - [X] implement POST /order
  - [X] test our client with the real extreme carpaccio server
  - [X] make sure we can parse the json order and build an order object
    - [X] make sure we can parse the json order
    - [X] use correct field to pass the body of the request?
    - [X] build an order object from the json (cf value_ field of the res object)
    - [X] Put the order object in a specific file
  - [X] Refactor handleRequest
  - [X] Return total amount json
  - [X] make sure we can easily build a json response (and play the game)
  - [X] By default return error 404 as answer to the request to be in line with other implementations
- [X] Implement the POST /feedback command
  - [X] check what the request, answers and behavior should look like from the js versions
  - [X] write a test to POST /feedback with a proper request
  - [X] implement POST /feedback
  - [X] test our client can receive feedback form the real extreme carpaccio server
- [ ] Add a configuration file for main options (ports and maybe others)
  - [ ] check the js client to see what options are available
  - [ ] create a config file of command line options to set these options

## PARKING
- [ ] Clean gitignore file in cpp
- [ ] Ajouter dans le cmake le Json en thirdparty pour la solution VS
- [ ] Add dependencies on sub boost headers only libraries (beast), instead on depending on full boost
- [ ] Fix that : Failed to find all ICU components (missing: ICU_INCLUDE_DIR _ICU_REQUIRED_LIBS_FOUND)
- [ ] Upgrade nlohman json version