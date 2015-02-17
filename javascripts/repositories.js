'use strict';

var _ = require('lodash');

var Sellers = function() {
    var sellersMap = {};

    this.all = function() {
        return _.map(sellersMap, function(seller) { return seller; });
    };

    this.add = function(seller) {
        sellersMap[seller.name] = seller;
    };

    this.get = function(sellerName) {
        return sellersMap[sellerName];
    }
};

Sellers.prototype = {
    count: function() {
        return this.all().length;
    },

    isEmpty: function() {
        return this.count() === 0;
    },

    updateCash: function(sellerName, profit) {
        this.get(sellerName).cash += parseFloat(profit);
    }
};

var Countries = function() {};

Countries.prototype = (function() {
    var Country = function(name, tax) {
        this.name = name;
        this.tax = tax;
    };

    var europeanCountries = [
        new Country('BG', 1.1),
        new Country('CZ', 1.11),
        new Country('DK', 1.12),
        new Country('DE', 1.13),
        new Country('EE', 1.14),
        new Country('IE', 1.15),
        new Country('EL', 1.16),
        new Country('ES', 1.17),
        new Country('FR', 1.18),
        new Country('HR', 1.19),
        new Country('IT', 1.2),
        new Country('CY', 1.1),
        new Country('LV', 1.11),
        new Country('LT', 1.12),
        new Country('LU', 1.13),
        new Country('HU', 1.14),
        new Country('MT', 1.15),
        new Country('NL', 1.16),
        new Country('AT', 1.17),
        new Country('PL', 1.18),
        new Country('PT', 1.19),
        new Country('RO', 1.2),
        new Country('SI', 1.1),
        new Country('SK', 1.11),
        new Country('FI', 1.12),
        new Country('SE', 1.13),
        new Country('UK', 1.14)
    ];

    var countryMap = _.reduce(europeanCountries, function(map, country) {
        map[country.name] = country;
        return map;
    }, {});

    return {
        fromEurope: Object.keys(countryMap),

        tax: function(countryName) {
            return countryMap[countryName].tax;
        }
    };
})();

var Reductions = function() {};

Reductions.prototype = (function (){
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
        reductionFor: function(total) {
            var reduc = _.result(_.find(reductions, function(reduc) { return reduc.sum < total; }), 'reduction');

            if(reduc == undefined)
                return 0;

            return reduc;
        }
    };
})();


exports = module.exports;

exports.Sellers = Sellers;
exports.Countries = Countries;
exports.Reductions = Reductions;