'use strict';

var repositories = require('../javascripts/repositories'),
    _ = require('lodash');
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

    it('should return all sellers sorted by cash in decreasing order', function() {
        sellers.add(bob);
        var alice = {name: 'alice', hostname: 'hostname', port: '3001', path: '/path', cash: 10.0};
        sellers.add(alice);
        var carol = {name: 'carol', hostname: 'hostname', port: '3002', path: '/path', cash: 5.0};
        sellers.add(carol);

        expect(sellers.all()[0]).toEqual(alice);
        expect(sellers.all()[1]).toEqual(carol);
        expect(sellers.all()[2]).toEqual(bob);
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

    it('should track cash evolution on cash update by iteration', function() {
        sellers.add(bob);

        sellers.updateCash('bob', 100, 0);

        expect(sellers.cashHistory).toEqual({'bob': [100]});
    });

    it('should track cash evolution on cash update by iteration and fill missing iterations with last value', function() {
        sellers.add(bob);

        sellers.updateCash('bob', 100, 3);
        sellers.updateCash('bob', 100, 4);

        expect(sellers.cashHistory).toEqual({'bob': [0, 0, 0, 100, 200]});
    });
});

describe('Countries', function() {
    var countries;

    beforeEach(function() {
        countries = new Countries();
    });

    it('should get the corresponding tax for a given country', function() {
        expect(countries.tax('DE')).toBe(1.2);
        expect(countries.tax('UK')).toBe(1.21);
        expect(countries.tax('FR')).toBe(1.2);
        expect(countries.tax('IT')).toBe(1.25);
        expect(countries.tax('ES')).toBe(1.19);
        expect(countries.tax('PL')).toBe(1.21);
        expect(countries.tax('RO')).toBe(1.2);
        expect(countries.tax('NL')).toBe(1.2);
        expect(countries.tax('BE')).toBe(1.24);
        expect(countries.tax('EL')).toBe(1.2);
        expect(countries.tax('CZ')).toBe(1.19);
        expect(countries.tax('PT')).toBe(1.23);
        expect(countries.tax('HU')).toBe(1.27);
        expect(countries.tax('SE')).toBe(1.23);
        expect(countries.tax('AT')).toBe(1.22);
        expect(countries.tax('BG')).toBe(1.21);
        expect(countries.tax('DK')).toBe(1.21);
        expect(countries.tax('FI')).toBe(1.17);
        expect(countries.tax('SK')).toBe(1.18);
        expect(countries.tax('IE')).toBe(1.21);
        expect(countries.tax('HR')).toBe(1.23);
        expect(countries.tax('LT')).toBe(1.23);
        expect(countries.tax('SI')).toBe(1.24);
        expect(countries.tax('LV')).toBe(1.2);
        expect(countries.tax('EE')).toBe(1.22);
        expect(countries.tax('CY')).toBe(1.21);
        expect(countries.tax('LU')).toBe(1.25);
        expect(countries.tax('MT')).toBe(1.2);
    });

    it('should return random country according to its frequency', function() {
        var mostImportantPopulation = 200000,
            samples = _.times(mostImportantPopulation * 10, countries.randomOne);

        var occurrences = _.groupBy(samples);

        expect(_.size(occurrences['FR'])).toBeGreaterThan(151381);
        expect(_.size(occurrences['UK'])).toBeGreaterThan(152741);
        expect(_.size(occurrences['LT'])).toBeLessThan(6844 * 10);
        expect(_.size(occurrences['NL'])).toBeLessThan(39842 * 10);
    });
});

describe('Reductions', function() {
    var reductions;

    beforeEach(function() {
        reductions = new Reductions();
    });

    it('should de reduced by 15% when total is bigger than 50,000', function() {
        expect(reductions.reductionFor(50001)).toBe(0.15);
    });

    it('should de reduced by 10% when total is between [10,000, 50,000)', function() {
        expect(reductions.reductionFor(10000)).toBe(0.10);
        expect(reductions.reductionFor(10500)).toBe(0.10);
    });

    it('should de reduced by 7% when total is between [7,000, 10,000)', function() {
        expect(reductions.reductionFor(7000)).toBe(0.07);
        expect(reductions.reductionFor(7500)).toBe(0.07);
    });

    it('should be reduced by 5% when total is between [5,000, 7,000)', function() {
        expect(reductions.reductionFor(5000)).toBe(0.05);
        expect(reductions.reductionFor(5500)).toBe(0.05);
    });

    it('should be reduced by 3% when total is between [1,000, 5,000)', function() {
        expect(reductions.reductionFor(1000)).toBe(0.03);
        expect(reductions.reductionFor(1100)).toBe(0.03);
    });

    it('should not be reduced when when the total between [0, 1,000)', function() {
        expect(reductions.reductionFor(500)).toBe(0.00);
    });
});
