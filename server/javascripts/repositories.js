'use strict';

var _ = require('lodash');

var Sellers = function() {
    var sellersMap = {};
    this.cashHistory = {};

    this.all = function() {
        var sellers = _.map(sellersMap, function (seller) {
            return seller;
        });
        return _.sortBy(sellers, function(seller) {return -seller.cash});
    };

    this.save = function(seller) {
        var previousCash;
        if(sellersMap[seller.name] === undefined) {
            sellersMap[seller.name] = seller;
            this.cashHistory[seller.name] = [];
        } else {
            previousCash = sellersMap[seller.name].cash;
            sellersMap[seller.name] = seller;
            sellersMap[seller.name].cash = previousCash;
        }
    };

    this.get = function(sellerName) {
        return sellersMap[sellerName];
    }
};

Sellers.prototype = (function(){
    function getLastRecordedCashAmount(currentSellersCashHistory, lastRecordedIteration) {
        var lastRecordedValue = currentSellersCashHistory[lastRecordedIteration - 1];

        if (lastRecordedValue === undefined) {
            lastRecordedValue = 0;
        }

        return lastRecordedValue;
    }

    function enlargeHistory(newSize, oldHistory) {
        var newHistory = new Array(newSize);
        newHistory.push.apply(newHistory, oldHistory);
        return newHistory;
    }

    function fillMissingIterations(currentIteration, currentSellersCashHistory) {
        var lastRecordedIteration = currentSellersCashHistory.length;

        if (lastRecordedIteration >= currentIteration) {
            return currentSellersCashHistory;
        }

        var newSellersCashHistory = enlargeHistory(currentIteration, currentSellersCashHistory);
        var lastRecordedValue = getLastRecordedCashAmount(currentSellersCashHistory, lastRecordedIteration);
        return _.fill(newSellersCashHistory, lastRecordedValue, lastRecordedIteration, currentIteration)
    }

    function updateCashHistory(self, seller, currentIteration) {
        var currentSellersCashHistory = self.cashHistory[seller.name];
        var newSellersCashHistory = fillMissingIterations(currentIteration, currentSellersCashHistory);
        newSellersCashHistory[currentIteration] = seller.cash;
        self.cashHistory[seller.name] = newSellersCashHistory;
    }

    return {
        count: function() {
            return this.all().length;
        },

        isEmpty: function() {
            return this.count() === 0;
        },

        updateCash: function(sellerName, amount, currentIteration) {
            var seller = this.get(sellerName);
            seller.cash += parseFloat(amount);
            updateCashHistory(this, seller, currentIteration);
        },

        setOffline: function(sellerName) {
            this.get(sellerName).online = false;
        },

        setOnline: function(sellerName) {
            this.get(sellerName).online = true;
        }
    }
})();

var Countries = function() {};

Countries.prototype = (function() {
    var Country = function(name, tax) {
        this.name = name;
        this.tax = tax;
    };

    var europeanCountries = {
        'DE': [1.2, 190995],
        'UK': [1.21, 152741],
        'FR': [1.2, 151381],
        'IT': [1.25, 143550],
        'ES': [1.19, 109023],
        'PL': [1.21, 90574],
        'RO': [1.2, 46640],
        'NL': [1.2, 39842],
        'BE': [1.24, 26510],
        'EL': [1.2, 25338],
        'CZ': [1.19, 24755],
        'PT': [1.23, 24261],
        'HU': [1.27, 23141],
        'SE': [1.23, 23047],
        'AT': [1.22, 20254],
        'BG': [1.21, 16905],
        'DK': [1.21, 13348],
        'FI': [1.17, 12903],
        'SK': [1.18, 12767],
        'IE': [1.21, 10894],
        'HR': [1.23, 9952],
        'LT': [1.23, 6844],
        'SI': [1.24, 4858],
        'LV': [1.2, 4656],
        'EE': [1.22, 3094],
        'CY': [1.21, 2],
        'LU': [1.25, 1],
        'MT': [1.2, 1]
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

var exports = module.exports;

exports.Sellers = Sellers;
exports.Countries = Countries;
