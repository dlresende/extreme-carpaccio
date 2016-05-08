var _ = require('lodash'),
    Reduction = require('./reduction'),
    utils = require('../utils'),
    colors = require('colors');

var BadRequest = function () {
    this.sendBadRequest = true;
    this.sendBadRequestPeriod = 3;
    this.modes = [0,1,2,3,4,5,6,7,8,9,10];
};

var Dispatcher = function(_sellerService, _orderService, _configuration) {
    this.sellerService = _sellerService ;
    this.orderService = _orderService;
    this.configuration = _configuration;
    this.offlinePenalty = 0;
    this.badRequest = new BadRequest();
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
                    console.info(colors.grey(seller.name + ' replied "' + sellerResponse + '"'));

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

    function putSellerOffline(self, seller, currentIteration) {
        return function() {
            console.error(colors.red('Could not reach seller ' + utils.stringify(seller)));
            var offlinePenalty = getConfiguration(self).offlinePenalty;

            if (! _.isNumber(offlinePenalty)) {
                console.warn(colors.yellow('Offline penalty is missing or is not a number. Using 0.'));
                offlinePenalty = 0;
            }

            self.sellerService.setOffline(seller, offlinePenalty, currentIteration);
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

        if(reductionStrategy !== 'STANDARD') {
            console.warn(colors.yellow('Unknown reduction strategy ' + reductionStrategy + '. Using STANDARD.'));
        }

        return new Period(Reduction.STANDARD, 5000);
    }

    function scheduleNextIteration(self, nextIteration, intervalInMillis) {
        setTimeout(function () {
            self.startBuying(nextIteration);
        }, intervalInMillis);
    }


    function getConfiguration(self) {
        return self.configuration.all();
    }

    return {
        sendOrderToSellers: function(reduction, currentIteration, badRequest) {
            var self = this,
                order = self.orderService.createOrder(reduction),
                expectedBill = self.orderService.bill(order, reduction);

            if(badRequest) {
                order = self.badRequest.corruptOrder(order);
            }

            _.forEach(self.sellerService.allSellers(), function(seller) {
                self.sellerService.addCash(seller, 0, currentIteration);
                var cashUpdater;
                if(badRequest) {
                    cashUpdater = updateSellersCash(self, seller, expectedBill, currentIteration);
                }
                else {
                    cashUpdater = self.badRequest.updateSellersCash(self, seller, expectedBill, currentIteration);
                }

                var errorCallback = putSellerOffline(self, seller, currentIteration);
                self.orderService.sendOrder(seller, order, cashUpdater, errorCallback);
            });
        },

        startBuying: function(iteration) {
            var reductionStrategy = getConfiguration(this).reduction,
                period = getReductionPeriodFor(reductionStrategy),
                badRequest = this.badRequest.shouldSendBadRequest(iteration),
                message = '>>> Shopping iteration ' + iteration;

            if(badRequest) {
                message = message + ' (bad request)';
            }
            console.info(colors.green(message));
            
            this.sendOrderToSellers(period.reduction, iteration);
            scheduleNextIteration(this, iteration + 1, period.shoppingIntervalInMillis);
        }
    }
})();

BadRequest.prototype = (function () {

    return {
        shouldSendBadRequest: function (iteration) {
            return this.sendBadRequest && (iteration % this.sendBadRequestPeriod == 0);
        },

        corruptOrder: function (order) {
            var mode = _.sample(this.modes),
                copy = _.clone(order);

            console.info(colors.blue('corrupt mode ' + mode));

            switch (mode) {
                case 0:
                    return {};
                case 1:
                    return _.map(_.range(1200), function (i) {
                        return i % 2 == 0;
                    });
                case 2:
                    copy.quantities = {error: 'datacenter unreachable'};
                    return copy;
                case 3:
                    copy.quantities = copy.quantities.slice(1);
                    return copy;
                case 4:
                    copy.prices = copy.prices.slice(1);
                    return copy;
                case 5:
                    copy.country = 'Llanfairpwllgwyngyllgogerychwyrndrobwllllantysiliogogogoch';
                    return copy;
                case 6:
                    delete copy.country;
                    return copy;
                case 7:
                    delete copy.prices;
                    return copy;
                case 8:
                    delete copy.quantities;
                    return copy;
                case 9:
                    delete copy.reduction;
                    return copy;
                case 10:
                    return null;

            }
            return {};
        },

        updateSellersCash: function (self, seller, expectedBill, currentIteration) {
            return function (response) {
                var amount = expectedBill.total,
                    sellerService = self.sellerService,
                    message;

                if (response.statusCode !== 400) {
                    var loss = amount * .5;
                    message = 'Hey ' + seller.name + ' lose ' + loss + ' because he does not know how to handle correctly a bad request';
                    sellerService.deductCash(seller, loss, currentIteration);
                    sellerService.notify(seller, {'type': 'ERROR', 'content': message});
                }
                else {
                    message = 'Hey, ' + seller.name + ' earned ' + amount;
                    sellerService.addCash(seller, amount, currentIteration);
                    sellerService.notify(seller, {'type': 'INFO', 'content': message});
                }
            }
        }
    }
})();

module.exports = {
    Dispatcher: Dispatcher,
    BadRequest: BadRequest
};
