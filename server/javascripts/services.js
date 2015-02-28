'use strict';

var _ = require('lodash');
var url = require("url");

var repositories = require('./repositories');
var utils = require('./utils');

var countries = new repositories.Countries();
var sellers = new repositories.Sellers();
var reductions = new repositories.Reductions();

var SellerService = function(_sellers) {
    this.sellers = _sellers || sellers;
};

SellerService.prototype = {
    register: function (sellerUrl, name) {
        var parsedUrl = url.parse(sellerUrl);
        var seller = {
            name: name,
            hostname: parsedUrl.hostname,
            port: parsedUrl.port,
            path: parsedUrl.path,
            cash: 0.0
        };
        this.sellers.add(seller);
        console.info('New seller registered: ' + utils.stringify(seller))
    },

    all: function() {
        return this.sellers.all();
    },

    updateCash: function(seller, expectedBill, actualBill) {
        try {
            var totalExpectedBill = utils.fixPrecision(expectedBill.total, 2);
            var totalActualBill = utils.fixPrecision(actualBill.total, 2);

            if(actualBill && totalExpectedBill === totalActualBill) {
                var message = 'Hey, ' + seller.name + ' earned ' + totalExpectedBill;
                this.sellers.updateCash(seller.name, totalExpectedBill);
                this.notify(seller, {'type': 'INFO', 'content': message});
            }

            else {
                var message = 'Goddamn, ' + seller.name + ' replied ' + totalActualBill + ' but right answer was ' +  totalExpectedBill;
                this.notify(seller, {'type': 'ERROR', 'content': message});
            }
        }
        catch (exception) {
            this.notify(seller, {'type': 'ERROR', 'content': exception.message});
        }
    },

    notify: function(seller, message) {
        utils.post(seller.hostname, seller.port, seller.path + 'feedback', message);

        if(message.type === 'ERROR') {
            console.error('Notifying ' + seller.name + ': ' + message.content);
        } else {
            console.info('Notifying ' + seller.name + ': ' + message.content);
        }
    }
};

var OrderService = function() {
};

OrderService.prototype = {
    sendOrder: function(seller, order, cashUpdater) {
        console.info('Sending order ' + utils.stringify(order) + ' to seller ' + utils.stringify(seller));
        utils.post(seller.hostname, seller.port, seller.path, order, cashUpdater);
    },

    createOrder: function(numberOfItems) {
        var items = numberOfItems || _.random(1, 10);
        var prices = new Array(items);
        var quantities = new Array(items);

        for(var item = 0; item < items; item++) {
            var price = _.random(1, 100, true);
            prices[item] = utils.fixPrecision(price, 2);
            quantities[item] = _.random(1, 10);
        }

        return {
            prices: prices,
            quantities: quantities,
            country: countries.randomOne()
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
    },

    validateBill: function(bill) {
        if(!_.has(bill, 'total')) {
            throw {message: 'The field \"total\" in the response is missing.'};
        }

        if(!_.isNumber(bill.total)) {
            throw {message: '\"Total\" is not a number.'};
        }
    }
};

var Dispatcher = function(_sellerService, _orderService) {
    this.Sellers = _sellerService || exports.SellerService;
    this.OrderService = _orderService || exports.OrderService;
};

Dispatcher.prototype = {
    sendOrderToSellers: function() {
        var orderService = this.OrderService;
        var sellerService = this.Sellers;

        var order = orderService.createOrder();
        var expectedBill = orderService.bill(order);

        function cashUpdater(seller) {
            return function(response) {
                if(response.statusCode === 200) {
                    response.on('error', function(err) {
                        console.error(err);
                    });
                    response.on('data', function (sellerResponse) {
                        try {
                            var actualBill = utils.jsonify(sellerResponse);
                            orderService.validateBill(actualBill);
                            sellerService.updateCash(seller, expectedBill, actualBill);
                        } catch(exception) {
                            sellerService.notify(seller, {'type': 'ERROR', 'content': exception.message});
                        }
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
        console.info('Purchasing round ' + iteration);

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
