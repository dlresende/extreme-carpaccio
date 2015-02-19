'use strict';

var Utils = function() {};

Utils.prototype = {
    stringify: function(object) {
        return JSON.stringify(object);
    },

    jsonify: function(string) {
        return JSON.parse(string);
    }
};

module.exports = new Utils();