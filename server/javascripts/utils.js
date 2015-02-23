'use strict';

var Utils = function() {};

Utils.prototype = {
    stringify: function(object) {
        return JSON.stringify(object);
    },

    jsonify: function(string) {
        try {
            return JSON.parse(string);
        }
        catch (exception) {
            console.log(exception);
            return {};
        }
    },

    fixPrecision: function(number, precision) {
        try {
            return parseFloat(number.toFixed(precision));
        }
        catch (exception) {
            console.log(exception);
            return 0.00;
        }
    }
};

module.exports = new Utils();