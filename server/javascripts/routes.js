var express = require('express'),
    _ = require('lodash');

module.exports = function (sellerService) {
    var router = express.Router();

    router.get('/sellers', function(request, response) {
        response.send('sellers', sellerService.allSellers() );
    });

    router.post('/sellers', function(request, response) {
        var sellerName = request.body.name,
            sellerUrl = request.body.url;
        if(_.isEmpty(sellerName) || _.isEmpty(sellerUrl)) {
          response.status(400).send({"message":"missing name or url"});
        }
        else {
          sellerService.register(sellerUrl, sellerName);
          response.status(200).end();
        }
    });

    return router;
};
