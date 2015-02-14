'use strict';

var httpDefault = require('http');
var _ = require('lodash');
var repositories = require('./repositories');

var OrderService = function(http) {
    this.http = http || httpDefault;
    this.countries = new repositories.Countries();
};

OrderService.prototype = {
    sendOrder: function(seller, order) {
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
        request.write(order);
        request.end();
    },

    createOrder: function(numberOfItems) {
        var items = numberOfItems || _.random(1, 100);
        var prices = new Array(items);
        var quantities = new Array(items);

        for(var item = 0; item < items; item++) {
            prices[item] = _.random(1, 1000, true).toFixed(2);
            quantities[item] = _.random(1, 100);
        }

        return {
            prices: prices,
            quantities: quantities,
            country: _.sample(this.countries.fromEurope)
        };
    }
};

exports = module.exports;

exports.OrderService = OrderService;