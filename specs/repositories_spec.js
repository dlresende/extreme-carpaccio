'use strict';

var repositories = require('../javascripts/repositories');
var Sellers = repositories.Sellers;
var Countries = repositories.Countries;
var Reductions = repositories.Reductions;

describe('Sellers', function(){
    var bob;
    var sellers;

    beforeEach(function(){
        bob = {name: 'bob', hostname: 'hostname', port: '3000', path: '/path', cash: 0.0};
        sellers = new Sellers();
    });

    it('should add sellers', function() {
        sellers.add(bob);

        expect(sellers.all()).toContain(bob);
    });

    it('should count sellers', function() {
        expect(sellers.count()).toBe(0);

        sellers.add(bob);

        expect(sellers.count()).toBe(1);
    });

    it('should say when there are sellers or not', function() {
        expect(sellers.isEmpty()).toBeTruthy();

        sellers.add(bob);

        expect(sellers.isEmpty()).toBeFalsy();
    });

    it('should update seller\'s cash', function() {
        sellers.add(bob);

        sellers.updateCash('bob', 100);

        expect(sellers.get('bob').cash).toBe(100);
    });
});

describe('Countries', function() {
    var countries;

    beforeEach(function() {
        countries = new Countries();
    });

    it('should get the corresponding tax for a given country', function() {
        expect(countries.tax('BG')).toBe(1.1);
        expect(countries.tax('CZ')).toBe(1.11);
        expect(countries.tax('DK')).toBe(1.12);
        expect(countries.tax('DE')).toBe(1.13);
        expect(countries.tax('EE')).toBe(1.14);
        expect(countries.tax('IE')).toBe(1.15);
        expect(countries.tax('EL')).toBe(1.16);
        expect(countries.tax('ES')).toBe(1.17);
        expect(countries.tax('FR')).toBe(1.18);
        expect(countries.tax('HR')).toBe(1.19);
        expect(countries.tax('IT')).toBe(1.2);
        expect(countries.tax('CY')).toBe(1.1);
        expect(countries.tax('LV')).toBe(1.11);
        expect(countries.tax('LT')).toBe(1.12);
        expect(countries.tax('LU')).toBe(1.13);
        expect(countries.tax('HU')).toBe(1.14);
        expect(countries.tax('MT')).toBe(1.15);
        expect(countries.tax('NL')).toBe(1.16);
        expect(countries.tax('AT')).toBe(1.17);
        expect(countries.tax('PL')).toBe(1.18);
        expect(countries.tax('PT')).toBe(1.19);
        expect(countries.tax('RO')).toBe(1.2);
        expect(countries.tax('SI')).toBe(1.1);
        expect(countries.tax('SK')).toBe(1.11);
        expect(countries.tax('FI')).toBe(1.12);
        expect(countries.tax('SE')).toBe(1.13);
        expect(countries.tax('UK')).toBe(1.14);
    });
});

var _ = require('lodash');


describe('Reductions', function() {
    var reductions;

    beforeEach(function() {
        reductions = new Reductions();
    });

    it('should get the corresponding reduction for a total', function() {
        expect(reductions.reductionFor(5500)).toBe(0.05);
    });

    it('should return 0 when the total is less than 1000', function() {
        expect(reductions.reductionFor(500)).toBe(0.00);
    });

    it('should return 0.15 when the total is 55000', function() {
        expect(reductions.reductionFor(55000)).toBe(0.15);
    });
});