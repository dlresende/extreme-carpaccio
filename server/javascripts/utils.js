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
            throw {message: 'The object ' + string + ' is not a valid json object.'};
        }
    },

    fixPrecision: function(number, precision) {
        return parseFloat(number.toFixed(precision));
    }
};

module.exports = new Utils();