# RETROSPECTIVE

05/27/2021
07/15/2021
09/02/2021

## What did we do ?
- We did some cmake and CPM
- We wanted to build a extreme carpaccio client in cpp
- We have all dependencies to create an answer to server
- We updated CPM to latest version
- We made a copy of Bowling game kata to have all cmake config ready
- We debugged requests format using JS version of carpaccio
07/15/2021
- We changed our approach, we chose to get the full example code and tested it successfully
- We change the allocator by using the default allocator, and it worked
09/02/2021
- We built a test on the client's http server using a get request
- We started to create class structure of server to host ioc, so that we can call start/stop on it
- 

## What did we learn ?
- Patrice learnt on client/server problematic and REST requests
07/15/2021
- We learnt that folder and file need to exist to be able to retrieve them from the server
- Starting with something that works and try to refactoring seems to be a good solution
09/02/2021

## Puzzle
- Dependencies handling in cpp is sport !
- We have the boost example of GET, will it work with POST ?
- Can we use boost to build the server ?
07/15/2021
- boost beast is a bit tricky to understand and troubleshoot
- Example of server using boost library is not real good c++ !!
09/02/2021
- We had some issues with threads. How can we stop server in a clean way ? How can we remove sleep ?

## Decide
- We miss some libraries in boost cmake so we need to add tham to build simple http client with boost
- We will add a Jira ticket to continue working on that
- 07/15/2021
09/02/2021
