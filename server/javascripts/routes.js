var express = require('express'),
    _ = require('lodash');

module.exports = function (sellerService, dispatcher) {
  var router = express.Router();
  var OK = 200;
  var BAD_REQUEST = 400;
  var UNAUTHORIZED = 401;

  router.get('/sellers', function(request, response) {
    response.status(OK).send(sellerService.allSellers());
  });

  router.get('/sellers/history', function(request, response) {
    var chunk = request.query.chunk || 10;
    response.status(OK).send(sellerService.getCashHistory(chunk));
  });

  router.post('/seller', function(request, response) {
    var sellerName = request.body.name,
        sellerUrl = request.body.url,
        sellerPwd = request.body.password;

    if (_.isEmpty(sellerName) || _.isEmpty(sellerUrl) || _.isEmpty(sellerPwd)) {
      response.status(BAD_REQUEST).send({message:'missing name, password or url'});
    } else if(sellerService.isAuthorized(sellerName, sellerPwd)) {
      sellerService.register(sellerUrl, sellerName, sellerPwd);
      response.status(OK).end();
    } else {
      response.status(UNAUTHORIZED).send({message:'invalid name or password'});
    }
  });

  router.post('/reduction', function(request, response) {
    var reduction = request.body.reduction;

    if (_.isEmpty(reduction)) {
      response.status(BAD_REQUEST).send({message:'missing reduction type: STANDARD, HALF PRICE, PAY THE PRICE'});
    } else if (reduction !== "STANDARD" && reduction !== "PAY THE PRICE" && reduction !== "HALF PRICE") {
      response.status(BAD_REQUEST).send({message:'unknown reduction type: STANDARD, HALF PRICE, PAY THE PRICE'});
    } else {
      dispatcher.updateReductionStrategy(reduction);
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
