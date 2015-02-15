'use strict';

var Sellers = require('../javascripts/repositories').Sellers;

describe('Sellers', function(){
    var seller;
    var sellers;

    beforeEach(function(){
        seller = {hostname: 'hostname', port: '3000', path: '/path'};
        sellers = new Sellers();
    });

    it('should add sellers', function() {
        sellers.add(seller);

        expect(sellers.all).toContain(seller);
    });

    it('should count sellers', function() {
        expect(sellers.count()).toBe(0);

        sellers.add(seller);

        expect(sellers.count()).toBe(1);
    });

    it('should say when there are sellers or not', function() {
        expect(sellers.isEmpty()).toBeTruthy();

        sellers.add(seller);

        expect(sellers.isEmpty()).toBeFalsy();
    });
});