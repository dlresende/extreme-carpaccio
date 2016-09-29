[![build status](https://travis-ci.org/dlresende/extreme-carpaccio.svg?branch=master)]()

# Instructor for Facilitators

## Technical Instructions

### Requirements
- nodejs
- bower

## Install & Run

```
npm install
bower install
npm start
```
Start in debug mode (activate debug mode for `xcarpaccio:server`):

```
DEBUG=xcarpaccio:server npm start
```

## Functional Instructions
Extreme Carpaccio is intended to be played with Product Owner (PO) and Developers together. It can be played with only POs or Developers, but the strategies tend to be more biased following one or another perspective/mindset. The goal is to create teams that are going to compete against each other. The workshop has mainly 3 stages: slicing, implementing and retrospective, and normally takes between 1:30 and 3:00 hours.

At the beginning, the facilitator exposes the problem to be solved to the participants. The participants then form teams between 2-4 (ideally) and try to understand the problem and define an implementation strategy together.

Next, the facilitator launch the server and make sure all the teams are able exchange messages with the server. Once everybody is ready to start, the facilitator restart the server and teams start to play. During the session, the facilitator can activate some "constraints" through the [configuration.json file](https://github.com/dlresende/extreme-carpaccio/blob/master/server/configuration.json), like bugs, change reduction strategies, etc., in order to bring some chaos to the game and shake the score.

Finally, the facilitator take some time at the end to exchange with all the teams and try to figure out what worked well, what could be improved.

More details [here](https://diegolemos.net/2016/01/07/extreme-carpaccio/).
