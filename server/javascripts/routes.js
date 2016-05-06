var express = require('express'),
    _ = require('lodash');

module.exports = function (sellerService, dispatcher) {
  var router = express.Router();
  var OK = 200;
  var BAD_REQUEST = 400;

  router.get('/sellers', function(request, response) {
    response.status(OK).send(sellerService.allSellers());
  });

  router.get('/sellers/history', function(request, response) {
    var chunk = request.query.chunk || 10;
    response.status(OK).send(sellerService.getCashHistory(chunk));
  });

  router.post('/seller', function(request, response) {
    var sellerName = request.body.name,
    sellerUrl = request.body.url;

    if (_.isEmpty(sellerName) || _.isEmpty(sellerUrl)) {
      response.status(BAD_REQUEST).send({message:'missing name or url'});
    } else {
      sellerService.register(sellerUrl, sellerName);
      response.status(OK).end();
    }
  });

  router.post('/offlinePenalty', function(request, response) {
    var penalty = request.body.penalty;

    if (! _.isNumber(penalty)) {
      response.status(BAD_REQUEST).send({message:'penalty is missing or is not a number'});
    } else {
      dispatcher.updateOfflinePenalty(penalty);
      response.status(OK).end();
    }
  });

  return router;
};
