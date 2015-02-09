var express = require('express');
var sellers = require('../public/javascripts/services').sellers;
var router = express.Router();

router.get('/', function(req, res) {
    res.render('sellers', { title: 'Sellers', sellers: sellers.all});
});

router.post('/', function(req, res) {
    var userUrl = req.body.url;
    sellers.register(userUrl);
    res.render('sellers', { title: 'Sellers', sellers: sellers.all});
});

module.exports = router;
