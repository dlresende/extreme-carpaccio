[![build status](https://travis-ci.org/dlresende/extreme-carpaccio.svg?branch=master)]()

# Instructions for Facilitators

## Requirements
- [nodejs](https://nodejs.org/en/)

## Install & Run

```
npm install
npm start
```
Start in debug mode (activate debug mode for `xcarpaccio:server`):

```
DEBUG=xcarpaccio:server npm start
```

## Workshop
Extreme Carpaccio is intended to be played with Product Owners (PO) and Developers together. It can be played with only Developers, but the strategies tend to be more biased following one or another perspective/mindset.

The goal is to create teams that are going to compete against each other. The workshop has mainly 3 stages: slicing, implementing and retrospective, and normally takes between 1:30 and 3:00 hours.

At the beginning, the facilitator exposes the problem to be solved to the participants. The participants then form teams between 2-4 (ideally) and try to understand the problem and define an implementation strategy together.

Next, the facilitator launches the server and makes sure all the teams are able to exchange messages with the server. Once everybody is ready to start, the facilitator restarts the server and teams start to play.

During the session, the facilitator can activate some "constraints" via the [configuration.json file](https://github.com/dlresende/extreme-carpaccio/blob/master/server/configuration.json), in order to bring some chaos to the game and shake the score. Some examples are: send bad requests (in which case participants should respond 400 - bad request); change reduction strategies; change tax rules; charge downtime; etc. Any change to this file is automatically taken into account, no need to restart the server.

Finally, the facilitator takes some time at the end to exchange with all the teams and try to figure out what worked well and what could be improved.

More details [here](https://diegolemos.net/2016/01/07/extreme-carpaccio/).
