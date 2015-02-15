'use strict';

var Sellers = function() {
    this.all = [];
};


Sellers.prototype = {
    add: function(seller) {
        this.all.push(seller);
    },

    count: function() {
        return this.all.length;
    },

    isEmpty: function() {
        return this.count() === 0;
    }
};

var Countries = function() {
    this.fromEurope = ['BG', 'CZ', 'DK', 'DE', 'EE', 'IE', 'EL', 'ES', 'FR', 'HR', 'IT', 'CY', 'LV', 'LT', 'LU', 'HU', 'MT', 'NL', 'AT', 'PL', 'PT', 'RO', 'SI', 'SK', 'FI', 'SE', 'UK'];
};

exports = module.exports;

exports.Sellers = Sellers;
exports.Countries = Countries;