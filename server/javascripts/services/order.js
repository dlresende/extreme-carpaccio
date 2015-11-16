var repositories = require('../repositories');
var countries = new repositories.Countries();
var _ = require('lodash');
var utils = require('../utils');

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



module.exports = OrderService;
