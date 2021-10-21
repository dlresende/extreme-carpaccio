# TO DO LIST

## TODO
- [X] Write a test sending an http request
  - [X] Deal with all the cmake arcane...
- [X] Create a main exe that starts a server 
- [X] Manual test that it can display a bmp, test it in the browser
- [X] Update our test to check that client can get a file from the server
- [X] Deal with starting & stopping the server correctly in the test to avoid resource leaks
- [ ] Implement the POST /order command
  - [X] check what the request and answers should look like from the js versions
  - [X] write a test to POST /order with a proper request
  - [X] implement POST /order
  - [ ] test our client with the real extreme carpaccio server
  - [ ] make sure we can easily build a json response (and play the game)
- [ ] Implement the POST /feedback command
  - [ ] check what the request, answers and behavior should look like from the js versions
  - [ ] write a test to POST /feedback with a proper request
  - [ ] implement POST /feedback
  - [ ] test our client can receive feedback form the real extreme carpaccio server
- [ ] Add a configuration file for main options (ports and maybe others)
  - [ ] check the js client to see what options are available
  - [ ] create a config file of command line options to set these options

## PARKING
- [ ] Clean gitignore file in cpp
- [ ] Ajouter dans le cmake le Json en thirdparty pour la solution VS
- [ ] Add dependencies on sub boost headers only libraries (beast), instead on depending on full boost
- [ ] Fix that : Failed to find all ICU components (missing: ICU_INCLUDE_DIR _ICU_REQUIRED_LIBS_FOUND)