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

exports = module.exports;

exports.Sellers = Sellers;