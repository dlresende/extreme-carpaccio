'use strict';

var _ = require('lodash');

var Sellers = function() {
    var sellersMap = {};

    this.all = function() {
        var sellers = _.map(sellersMap, function (seller) {
            return seller;
        });
        return _.sortBy(sellers, function(seller) {return -seller.cash});
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

    var europeanCountries = {
        'BG': [1.1, 1],
        'CZ': [1.11, 1],
        'DK': [1.12, 1],
        'DE': [1.13, 1],
        'EE': [1.14, 1],
        'IE': [1.15, 1],
        'EL': [1.16, 1],
        'ES': [1.17, 1],
        'FR': [1.18, 20],
        'HR': [1.19, 1],
        'IT': [1.2,  1],
        'CY': [1.1,  1],
        'LV': [1.11, 1],
        'LT': [1.12, 1],
        'LU': [1.13, 1],
        'HU': [1.14, 1],
        'MT': [1.15, 1],
        'NL': [1.16, 1],
        'AT': [1.17, 1],
        'PL': [1.18, 1],
        'PT': [1.19, 1],
        'RO': [1.2,  1],
        'SI': [1.1,  1],
        'SK': [1.11, 1],
        'FI': [1.12, 1],
        'SE': [1.13, 1],
        'UK': [1.14, 20]
    };

    var countryDistributionByWeight = _.reduce(europeanCountries, function(distrib, infos, country) {
        var i;
        for(i=0; i<infos[1]; i++) {
            distrib.push(country);
        }
        return distrib;
    }, []);
    _.shuffle(countryDistributionByWeight);

    var countryMap = _.reduce(europeanCountries, function(map, infos, country) {
        map[country] = new Country(country, infos[0]);
        return map;
    }, {});

    return {
        fromEurope: Object.keys(countryMap),

        randomOne: function() {
            return _.sample(countryDistributionByWeight);
        },

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
            var reduction = _.result(_.find(reductions, function(reduc) { return reduc.sum <= total; }), 'reduction');

            if(reduction == undefined) {
                return 0;
            }

            return reduction;
        }
    };
})();


exports = module.exports;

exports.Sellers = Sellers;
exports.Countries = Countries;
exports.Reductions = Reductions;
