var _ = require('lodash');

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

module.exports = Dispatcher;
