var express = require('express'),
    _ = require('lodash');

module.exports = function (sellerService, dispatcher) {
  var router = express.Router();
  var OK = 200;
  var BAD_REQUEST = 400;
  var UNAUTHORIZED = 401;

  router.get('/sellers', function(request, response) {
    // seller view is returned, to prevent any confidential information leaks
    var sellerViews = _.map(sellerService.allSellers(), function(seller) {
      return {
        cash: seller.cash,
        name: seller.name,
        online: seller.online
      };
    });
    response.status(OK).send(sellerViews);
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
