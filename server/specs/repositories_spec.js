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
        sellers.save(bob);
        var alice = {name: 'alice', hostname: 'hostname', port: '3001', path: '/path', cash: 10.0};
        sellers.save(alice);
        var carol = {name: 'carol', hostname: 'hostname', port: '3002', path: '/path', cash: 5.0};
        sellers.save(carol);

        expect(sellers.all()[0]).toEqual(alice);
        expect(sellers.all()[1]).toEqual(carol);
        expect(sellers.all()[2]).toEqual(bob);
    });

    it('should add sellers', function() {
        sellers.save(bob);

        expect(sellers.all()).toContain(bob);
    });

    it('should update sellers data and preserve cash & cash history', function() {
        var newBob, updatedBob;
        sellers.save(bob);
        sellers.updateCash(bob.name, 42, 1);

        newBob = {name: bob.name, hostname: 'new hostname'};
        sellers.save(newBob);

        expect(sellers.all().length).toBe(1);
        updatedBob = sellers.get(bob.name);
        expect(updatedBob.hostname).toEqual('new hostname');
        expect(updatedBob.cash).toBe(42);
        expect(sellers.cashHistory).toEqual({'bob': [0, 42]});
    });

    it('should count sellers', function() {
        expect(sellers.count()).toBe(0);

        sellers.save(bob);

        expect(sellers.count()).toBe(1);
    });

    it('should say when there are sellers or not', function() {
        expect(sellers.isEmpty()).toBeTruthy();

        sellers.save(bob);

        expect(sellers.isEmpty()).toBeFalsy();
    });

    it('should update seller\'s cash', function() {
        sellers.save(bob);

        sellers.updateCash('bob', 100);

        expect(sellers.get('bob').cash).toBe(100);
    });

    it('should track cash evolution on cash update by iteration', function() {
        sellers.save(bob);

        sellers.updateCash('bob', 100, 0);

        expect(sellers.cashHistory).toEqual({'bob': [100]});
    });

    it('should track cash evolution on cash update by iteration and fill missing iterations with last value', function() {
        sellers.save(bob);

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

