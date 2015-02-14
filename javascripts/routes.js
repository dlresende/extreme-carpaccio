var express = require('express');
var repositories = require('./repositories');

var sellers = new repositories.Sellers();
var router = express.Router();

router.get('/', function(request, response) {
    response.render('index', { title: 'Katazon' });
});

router.get('/sellers', function(request, response) {
    response.render('sellers', { title: 'Sellers', sellers: sellers.all });
});

router.post('/sellers', function(request, response) {
    var sellerUrl = request.body.url;
    sellers.register(sellerUrl);
    response.render('sellers', { title: 'Sellers', sellers: sellers.all});
});

module.exports = router;
