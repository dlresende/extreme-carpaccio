'use strict';

var server = {
    sellers: [],

    register: function (url) {
        this.sellers.push(url);
    }
};

module.exports = server;
