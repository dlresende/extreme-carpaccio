'use strict';

var services = require('../public/javascripts/services');

describe('sellers', function(){
    var sellers = services.sellers;

    it('should register the users\' URL', function() {
        sellers.register('url');

        expect(sellers.all).toContain("url");
    });
});