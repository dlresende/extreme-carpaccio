var express = require('express');

module.exports = function(sellerService, orderService) {
    var router = express.Router();

    router.get('/', function(request, response) {
        response.render('index', { title: 'Katazon' });
    });

    router.get('/sellers', function(request, response) {
        console.log(sellerService.all());
        response.render('sellers', { title: 'Sellers', sellers: sellerService.all() });
    });

    router.post('/sellers', function(request, response) {
        var sellerUrl = request.body.url;
        sellerService.register(sellerUrl);
        response.render('sellers', { title: 'Sellers', sellers: sellerService.all()});
    });

    router.post('/test', function(request, response) {

        console.log(request.body);
        var bill = orderService.bill(request.body);
        response.send(bill);
    });
    return router;
};
