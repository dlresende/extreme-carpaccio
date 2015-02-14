'use strict';

var httpDefault = require('http');

var Dispatcher = function(http) {
    this.http = http || httpDefault;
};

Dispatcher.prototype = {
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
    }
};

exports = module.exports;

exports.Dispatcher = Dispatcher;