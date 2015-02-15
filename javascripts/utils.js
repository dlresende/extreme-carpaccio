'use strict';

var Utils = function() {};

Utils.prototype = {
    stringify: function(object) {
        return JSON.stringify(object);
    }
};

module.exports = new Utils();