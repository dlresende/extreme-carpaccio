'use strict';

var httpDefault = require('http');
var _ = require('lodash');
var url = require("url");

var repositories = require('./repositories');
var utils = require('./utils');

var countries = new repositories.Countries();
var sellers = new repositories.Sellers();

function fixPrecision(number, precision) {
    return parseFloat(number.toFixed(precision));
}

var SellerService = function(_sellers) {
    this.sellers = _sellers || sellers;
};

SellerService.prototype = {
    register: function (sellerUrl) {
        var parsedUrl = url.parse(sellerUrl);
        var seller = {
            hostname: parsedUrl.hostname,
            port: parsedUrl.port,
            path: parsedUrl.path
        };
        this.sellers.add(seller);
        console.log('New seller registered: ' + utils.stringify(seller))
    },

    all: function() {
        return this.sellers.all;
    }
};

var OrderService = function(_http) {
    this.http = _http || httpDefault;
};

OrderService.prototype = {
    sendOrder: function(seller, order) {
        var orderStringified = utils.stringify(order);
        console.log('Sending order: ' + orderStringified + ' to seller: ' + utils.stringify(seller));
        
        var options = {
            hostname: seller.hostname,
            port: seller.port,
            path: seller.path,
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            }
        };
        var request = this.http.request(options);
        request.write(orderStringified);
        request.end();
    },

    createOrder: function(numberOfItems) {
        var items = numberOfItems || _.random(1, 100);
        var prices = new Array(items);
        var quantities = new Array(items);

        for(var item = 0; item < items; item++) {
            var price = _.random(1, 1000, true);
            prices[item] = fixPrecision(price, 2);
            quantities[item] = _.random(1, 100);
        }

        return {
            prices: prices,
            quantities: quantities,
            country: _.sample(countries.fromEurope)
        };
    }
};

var Dispatcher = function(_sellers, _orderService) {
    this.Sellers = _sellers || sellers;
    this.OrderService = _orderService || exports.OrderService;
};

Dispatcher.prototype = {
    sendOrderToSellers: function() {
        var orderService = this.OrderService;
        var sellers = this.Sellers;

        if(sellers.isEmpty()) {
            console.log('No sellers currently registered.');
            return;
        }

        _.forEach(sellers.all, function(seller) {
            var order = orderService.createOrder();
            orderService.sendOrder(seller, order);
        });
    },

    startBuying: function(intervalInMillis, round) {
        var iteration = round || 1;
        console.log('Purchasing round ' + iteration);

        var self = this;
        self.sendOrderToSellers();

        setTimeout(function () {
            self.startBuying(intervalInMillis, iteration + 1);
        }, intervalInMillis);
    }
};

exports = module.exports;

exports.OrderService = OrderService;
exports.Dispatcher = Dispatcher;
exports.SellerService = SellerService;