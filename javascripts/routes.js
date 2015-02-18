var express = require('express');

module.exports = function (sellerService) {
    var router = express.Router();

    router.get('/', function(request, response) {
        response.render('index', { title: 'Katazon' });
    });

    router.get('/sellers', function(request, response) {
        response.render('sellers', { title: 'Sellers', sellers: sellerService.all() });
    });

    router.post('/sellers', function(request, response) {
        var sellerUrl = request.body.url;
        sellerService.register(sellerUrl);
        response.redirect('/sellers');
    });

    return router;
};
