'use strict';

var _ = require('lodash');
var url = require("url");

var repositories = require('./repositories');
var utils = require('./utils');

var countries = new repositories.Countries();
var sellers = new repositories.Sellers();

var SellerService = function(_sellers) {
    this.sellers = _sellers || sellers;
};
SellerService.prototype = {
    addCash: function(seller, amount, currentIteration) {
        this.sellers.updateCash(seller.name, amount, currentIteration);
    },
    deductCash: function(seller, amount, currentIteration) {
        this.sellers.updateCash(seller.name, -amount, currentIteration);
    },
    getCashHistory: function(chunk) {
        var cashHistory = this.sellers.cashHistory;
        var cashHistoryReduced = {};
        var lastIteration;

        var seller;
        for(seller in cashHistory) {
            cashHistoryReduced[seller] = [];

            var i = 0;
            for(; i < cashHistory[seller].length; i++) {
                if((i + 1) % chunk === 0) {
                    cashHistoryReduced[seller].push(cashHistory[seller][i]);
                }
            }

            if(i % chunk !== 0) {
                cashHistoryReduced[seller].push(cashHistory[seller][i - 1]);
            }

            lastIteration = i;
        }

        return {history: cashHistoryReduced, lastIteration: lastIteration};
    },

    register: function (sellerUrl, name) {
        var parsedUrl = url.parse(sellerUrl);
        var seller = {
            name: name,
            hostname: parsedUrl.hostname,
            port: parsedUrl.port,
            path: parsedUrl.path,
            cash: 0.0,
            online: false
        };
        this.sellers.add(seller);
        console.info('New seller registered: ' + utils.stringify(seller))
    },

    allSellers: function() {
        return this.sellers.all();
    },

    updateCash: function(seller, expectedBill, actualBill, currentIteration) {
        try {
            var totalExpectedBill = utils.fixPrecision(expectedBill.total, 2);
            var totalActualBill = utils.fixPrecision(actualBill.total, 2);

            if(actualBill && totalExpectedBill === totalActualBill) {
                this.addCash(seller, totalExpectedBill, currentIteration);
                this.notify(seller, {'type': 'INFO', 'content': 'Hey, ' + seller.name + ' earned ' + totalExpectedBill});
            }

            else {
                var loss = utils.fixPrecision(totalExpectedBill * .1, 2);
                this.deductCash(seller, loss, currentIteration);
                var message = 'Goddamn, ' + seller.name + ' replied ' + totalActualBill + ' but right answer was ' +  totalExpectedBill + '. ' + loss + ' will be charged.';
                this.notify(seller, {'type': 'ERROR', 'content': message});
            }
        }
        catch (exception) {
            this.notify(seller, {'type': 'ERROR', 'content': exception.message});
        }
    },

    setOffline: function(seller) {
        this.sellers.setOffline(seller.name);
    },

    setOnline: function(seller) {
        this.sellers.setOnline(seller.name);
    },

    notify: function(seller, message) {
        utils.post(seller.hostname, seller.port, seller.path + '/feedback', message);

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
    sendOrder: function(seller, order, cashUpdater, logError) {
        console.info('Sending order ' + utils.stringify(order) + ' to seller ' + utils.stringify(seller));
        utils.post(seller.hostname, seller.port, seller.path + '/order', order, cashUpdater, logError);
    },

    createOrder: function(reduction) {
        var items = _.random(1, 10);
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
            country: countries.randomOne(),
            reduction: reduction.name
        };
    },

    bill: function(order, reduction) {
        var sum = 0;

        for(var item = 0; item < order.prices.length; item++) {
            var price = order.prices[item];
            var quantity = order.quantities[item];
            sum += price * quantity;
        }

        var tax = countries.tax(order.country);
        sum = reduction.apply(sum * tax);

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

var Reduction = (function(){
    var PayThePriceReduction = function() {
        this.name = 'PAY THE PRICE';
    };
    PayThePriceReduction.prototype = {
        apply: function(amount) {
            return amount;
        }
    };

    var HalfPriceReduction = function() {
        this.name = 'HALF PRICE';
    };
    HalfPriceReduction.prototype = {
        apply: function(amount) {
            return amount * (1 - .5);
        }
    };

    var StandardReduction = function() {
        this.name = 'STANDARD';
    };
    StandardReduction.prototype = (function (){
        var Reduction = function(sum, reduction) {
            this.sum = sum;
            this.reduction = reduction;
        };

        var reductions = [
            new Reduction(50000, 0.15),
            new Reduction(10000, 0.10),
            new Reduction(7000, 0.07),
            new Reduction(5000, 0.05),
            new Reduction(1000, 0.03)
        ];

        return {
            apply: function(amount) {
                return amount * (1 - this.reductionFor(amount));
            },

            reductionFor: function(total) {
                var reduction = _.result(_.find(reductions, function(reduc) { return reduc.sum <= total; }), 'reduction');

                if(reduction === undefined) {
                    return 0;
                }

                return reduction;
            }
        };
    })();

    return {
        STANDARD : new StandardReduction(),
        PAY_THE_PRICE : new PayThePriceReduction(),
        HALF_PRICE: new HalfPriceReduction()
    }
})();

var Dispatcher = function(_sellerService, _orderService) {
    this.sellerService = _sellerService ;
    this.orderService = _orderService;
    this.reductionStrategy = 'STANDARD';
};
Dispatcher.prototype = (function() {
    function updateSellersCash(self, seller, expectedBill, currentIteration) {
        return function(response) {
            if(response.statusCode === 200) {
                self.sellerService.setOnline(seller);

                response.on('error', function(err) {
                    console.error(err);
                });

                response.on('data', function (sellerResponse) {
                    console.info(seller.name + ' replied "' + sellerResponse + '"');

                    try {
                        var actualBill = utils.jsonify(sellerResponse);
                        self.orderService.validateBill(actualBill);
                        self.sellerService.updateCash(seller, expectedBill, actualBill, currentIteration);
                    } catch(exception) {
                        self.sellerService.notify(seller, {'type': 'ERROR', 'content': exception.message});
                    }
                });
            }
        }
    }

    function logError(self, seller) {
        return function() {
            self.sellerService.setOffline(seller);
            console.error('Could not reach seller ' + utils.stringify(seller));
        }
    }

    var Period = function(reduction, shoppingIntervalInMillis) {
        this.reduction = reduction;
        this.shoppingIntervalInMillis = shoppingIntervalInMillis;
    };

    function getReductionPeriodFor(reductionStrategy) {
        if(reductionStrategy === 'PAY THE PRICE') {
            return new Period(Reduction.PAY_THE_PRICE,  10000);
        }

        if(reductionStrategy === 'HALF PRICE') {
            return new Period(Reduction.HALF_PRICE,  1000);
        }

        return new Period(Reduction.STANDARD, 5000);
    }

    function scheduleNextIteration(self, nextIteration, intervalInMillis) {
        setTimeout(function () {
            self.startBuying(nextIteration);
        }, intervalInMillis);
    }

    return {
        updateReductionStrategy: function(newReductionStrategy) {
            this.reductionStrategy = newReductionStrategy;
        },

        sendOrderToSellers: function(reduction, currentIteration) {
            var self = this;
            var order = self.orderService.createOrder(reduction);
            var expectedBill = self.orderService.bill(order, reduction);

            _.forEach(self.sellerService.allSellers(), function(seller) {
                self.sellerService.addCash(seller, 0, currentIteration);
                var cashUpdater = updateSellersCash(self, seller, expectedBill, currentIteration);
                var errorCallback = logError(self, seller);
                self.orderService.sendOrder(seller, order, cashUpdater, errorCallback);
            });
        },

        startBuying: function(iteration) {
            console.info('>>> Shopping iteration ' + iteration);

            var period = getReductionPeriodFor(this.reductionStrategy);
            this.sendOrderToSellers(period.reduction, iteration);
            scheduleNextIteration(this, iteration + 1, period.shoppingIntervalInMillis);
        }
    }
})();

var exports = module.exports;

exports.OrderService = OrderService;
exports.Dispatcher = Dispatcher;
exports.SellerService = SellerService;
exports.Reduction = Reduction;
