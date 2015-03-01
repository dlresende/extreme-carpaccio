'use strict';

var http = require('http');

var Utils = function(_http) {
    this.http = _http || http;
};

Utils.prototype = {
    stringify: function(object) {
        return JSON.stringify(object);
    },

    jsonify: function(string) {
        try {
            return JSON.parse(string);
        }
        catch (exception) {
            throw {message: 'The object "' + string + '" is not a valid json object'};
        }
    },

    fixPrecision: function(number, precision) {
        return parseFloat(number.toFixed(precision));
    },

    post: function(hostname, port, path, body, onSuccess, onError) {
        var bodyStringified = this.stringify(body);
        var options = {
            hostname: hostname,
            port: port,
            path: (path || '').replace("//", "/"),
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
                'Content-Length' : bodyStringified.length
            }
        };
        var request = this.http.request(options, onSuccess);
        request.on('error', onError || function() {});
        request.write(bodyStringified);
        request.end();
    }
};

module.exports = new Utils();