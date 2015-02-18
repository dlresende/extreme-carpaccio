'use strict';

var httpDefault = require('http');
var _ = require('lodash');
var url = require("url");

var repositories = require('./repositories');
var utils = require('./utils');

var countries = new repositories.Countries();
var sellers = new repositories.Sellers();
var reductions = new repositories.Reductions();

function fixPrecision(number, precision) {
    return parseFloat(number.toFixed(precision));
}

var SellerService = function(_sellers) {
    this.sellers = _sellers || sellers;
};

SellerService.prototype = {
    register: function (sellerUrl, name) {
        var parsedUrl = url.parse(sellerUrl);
        var seller = {
            name: name || parsedUrl.hostname,
            hostname: parsedUrl.hostname,
            port: parsedUrl.port,
            path: parsedUrl.path,
            cash: 0.0
        };
        this.sellers.add(seller);
        console.log('New seller registered: ' + utils.stringify(seller))
    },

    all: function() {
        return this.sellers.all();
    },

    updateCash: function(sellerName, expectedBill, actualBill) {
        var totalExpectedBill = fixPrecision(expectedBill.total, 2);
        var totalActualBill = fixPrecision(actualBill.total, 2);
        if(actualBill && totalExpectedBill === totalActualBill) {
            console.log('Hey, ' + sellerName + ' earned ' + totalExpectedBill);
            this.sellers.updateCash(sellerName, totalExpectedBill);
        }

        else {
            console.log('Goddamn, ' + sellerName + ' replied ' + actualBill.total + ' but right answer was ' + expectedBill.total);
        }
    }
};

var OrderService = function(_http) {
    this.http = _http || httpDefault;
};

OrderService.prototype = {
    sendOrder: function(seller, order, cashUpdater) {
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
        var request = this.http.request(options, cashUpdater);
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
    },

    bill: function(order) {
        var sum = 0;

        for(var item = 0; item < order.prices.length; item++) {
            var price = order.prices[item];
            var quantity = order.quantities[item];
            sum += price * quantity;
        }
        var reduction = reductions.reductionFor(sum);
        var tax = countries.tax(order.country);
        sum = sum * tax * (1 - reduction);
        return { total: sum };
    }
};

var Dispatcher = function(_sellerService, _orderService) {
    this.Sellers = _sellerService || exports.SellerService;
    this.orderService = _orderService || exports.orderService;
};

Dispatcher.prototype = {
    sendOrderToSellers: function() {
        var orderService = this.orderService;
        var sellerService = this.Sellers;

        var order = orderService.createOrder();
        var bill = orderService.bill(order);

        function cashUpdater(seller) {
            return function(response) {
                if(response.statusCode === 200) {
                    response.on('data', function (sellerResponse) {
                        sellerService.updateCash(seller.name, bill, utils.jsonify(sellerResponse));
                    });
                }
            }
        }

        _.forEach(sellerService.all(), function(seller) {
            orderService.sendOrder(seller, order, cashUpdater(seller));
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

exports.orderService = OrderService;
exports.Dispatcher = Dispatcher;
exports.SellerService = SellerService;