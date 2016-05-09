'use strict';

var _ = require('lodash'),
    colors = require('colors');

var Sellers = function() {
    var sellersMap = {};
    var cashHistory = {};

    this.cashHistory = cashHistory;

    this.all = function() {
        var sellers = _.map(sellersMap, function (seller) {
            return seller;
        });
        return _.sortBy(sellers, function(seller) {return -seller.cash});
    };

    this.save = function(seller) {
        if(sellersMap[seller.name] === undefined) {
            add(seller);
        } else {
            update(seller);
        }
    };

    this.get = function(sellerName) {
        return sellersMap[sellerName];
    };

    function add(seller) {
        sellersMap[seller.name] = seller;
        cashHistory[seller.name] = [];
    }
    function update(seller) {
        var previousCash = sellersMap[seller.name].cash;
        sellersMap[seller.name] = seller;
        sellersMap[seller.name].cash = previousCash;
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

var Countries = function(configuration) {
    this.configuration = configuration;
};

Countries.prototype = (function() {

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

    function scale(factor) {
        return function(price) { return price * factor };
    }

    function defaultTaxRule(name) {
        return scale(europeanCountries[name][0]);
    }

    var Country = function(name, taxRule) {
        this.name = name;
        this.taxRule = taxRule;
    };

    function customEval(s) { return new Function("return " + s)(); }

    function lookupForOverridenDefinition(configuration, country) {
        var conf = configuration.all();
        if(!conf.taxes || !conf.taxes[country]) {
            return null;
        }

        var def = conf.taxes[country];
        if(_.isNumber(def)) {
            console.info(colors.blue('Tax rule for country ' + country + ' changed to scale factor ' + def));
            return scale(def);
        }

        if(_.isString(def)) {
            try {
                var taxRule = customEval(def);
                if(_.isFunction(taxRule)) {
                    console.info(colors.blue('Tax rule for country ' + country + ' changed to function ' + def));
                    return taxRule;
                }
                else {
                    console.error(colors.red('Failed to evaluate tax rule for country ' + country + ' from ' + def + ', result is not a function'));
                    return null;
                }
            } catch (e) {
                console.error(colors.red('Failed to evaluate tax rule for country ' + country + ' from ' + def + ', got: ' + e));
                return null;
            }
        }

        return null;
    }

    Country.prototype = {
        withConfiguration: function(configuration) {
            this.configuration = configuration;
            return this;
        },
        applyTax : function(sum) {
            var newRule = lookupForOverridenDefinition(this.configuration, this.name);

            if(newRule==null) {
                return this.taxRule.apply(this, [sum]);
            }

            try {
                return newRule.apply(this, [sum]);
            } catch(e) {
                console.error(colors.red('Failed to evaluate tax rule for country ' + this.name + ' falling back to original value, got:' + e));
                return this.taxRule.apply(this, [sum]);
            }
        }
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
        map[country] = new Country(country, defaultTaxRule(country));
        return map;
    }, {});

    return {
        fromEurope: Object.keys(countryMap),

        randomOne: function() {
            return _.sample(countryDistributionByWeight);
        },

    	taxRule: function(countryName) {
            var country = countryMap[countryName];
            return country.withConfiguration(this.configuration);
        },

	    updateTax: function(country, taxRule) {
            countryMap[country].taxRule = taxRule;
	    }
    };
})();

module.exports = {
    Sellers: Sellers,
    Countries: Countries
};
