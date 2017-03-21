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
Extreme Carpaccio is intended to be played with Product Owners (PO) and Developers together. It can be played with only Developers, but slicing strategies tend to be more biased since developers generally focus more on code and than on product and iterations.

The workshop has mainly 3 stages: slicing, implementation and retrospective. A session normally takes between 1:30 and 3:00 hours.

At the beginning, the facilitator exposes the problem to be solved to participants. The participants then form teams between 2-4 (ideally) and try to understand and slice the problem, and together define an implementation strategy, based on product value perspective and technical challenges trade-offs.

Next, the facilitator starts the server and makes sure all the teams are able to exchange HTTP messages with the server. Once everyone is ready, the facilitator allows teams to start implementing (normally requires restarting the server to reset the score) and people start playing.

During the session, the facilitator can activate some "constraints" via the [configuration.json file](https://github.com/dlresende/extreme-carpaccio/blob/master/server/configuration.json), in order to bring some chaos to the game and shake the score. Some examples are: send bad requests (**in which case participants should respond 400 - bad request**); change reduction strategies; change tax rules; charge downtime; etc. Any change to this file is automatically taken into account, no need to restart the server. It is up to the facilitator to announce when he/she triggers a constraint, based on how he/she wants to conduct the session.

At the end, when the facilitator decides to stop the implementation session and the winner becomes known, he/she takes some time at to exchange with participants about the exercise: what worked well, what could be improved, feedbacks, learnings, etc.

I strongly encourage people facilitating or playing Extreme Carpaccio to tweet using the hashtag [#ExtremeCarpaccio](https://twitter.com/search?vertical=default&q=%22extreme%20carpaccio%22%20OR%20%22Xtreme%20carpaccio%22%20OR%20%23ExtremeCarpaccio&src=typd) with their impressions, feelings, feedbacks, etc. Needless to say, but just in case, feel free to fork, hack, make pull requests, talk about, blog, run the exercise on meetups, conferences, compagnies, etc.

More details about the exercise [here](https://diegolemos.net/2016/01/07/extreme-carpaccio/).
