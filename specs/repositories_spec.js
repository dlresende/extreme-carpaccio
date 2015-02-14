'use strict';

var Sellers = require('../javascripts/repositories').Sellers;

describe('repositories', function(){

    var sellers;

    beforeEach(function(){
        sellers = new Sellers();
    });

    it('should register the users\' URL', function() {
        sellers.register('http://hostname:3000/path');

        expect(sellers.all).toContain({hostname: 'hostname', port: '3000', path: '/path'});
    });
});