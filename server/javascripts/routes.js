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

  return router;
};
