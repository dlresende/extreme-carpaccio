'use strict';

var url = require("url");

var Sellers = function() {
    this.all = [];
};

Sellers.prototype = {
    register: function (sellerUrl) {
        var parsedUrl = url.parse(sellerUrl);
        var seller = {
            hostname: parsedUrl.hostname,
            port: parsedUrl.port,
            path: parsedUrl.path
        };
        this.all.push(seller);
    }
};

var Countries = function() {
    this.fromEurope = ['BG', 'CZ', 'DK', 'DE', 'EE', 'IE', 'EL', 'ES', 'FR', 'HR', 'IT', 'CY', 'LV', 'LT', 'LU', 'HU', 'MT', 'NL', 'AT', 'PL', 'PT', 'RO', 'SI', 'SK', 'FI', 'SE', 'UK'];
};

exports = module.exports;

exports.Sellers = Sellers;
exports.Countries = Countries;