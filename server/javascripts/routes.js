var express = require('express'),
    _ = require('lodash');

module.exports = function (sellerService) {
    var router = express.Router();
    var OK = 200;
    var BAD_REQUEST = 400;

    router.get('/sellers', function(request, response) {
        response.status(OK).send(sellerService.allSellers());
    });

    router.post('/seller', function(request, response) {
        var sellerName = request.body.name,
            sellerUrl = request.body.url;

        if(_.isEmpty(sellerName) || _.isEmpty(sellerUrl)) {
          response.status(BAD_REQUEST).send({message:'missing name or url'});
        } else {
          sellerService.register(sellerUrl, sellerName);
          response.status(OK).end();
        }
    });

    return router;
};
