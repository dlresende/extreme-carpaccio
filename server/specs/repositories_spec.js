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
        expect(countries.taxRule('DE').applyTax(1)).toBe(1.2);
        expect(countries.taxRule('UK').applyTax(1)).toBe(1.21);
        expect(countries.taxRule('FR').applyTax(1)).toBe(1.2);
        expect(countries.taxRule('IT').applyTax(1)).toBe(1.25);
        expect(countries.taxRule('ES').applyTax(1)).toBe(1.19);
        expect(countries.taxRule('PL').applyTax(1)).toBe(1.21);
        expect(countries.taxRule('RO').applyTax(1)).toBe(1.2);
        expect(countries.taxRule('NL').applyTax(1)).toBe(1.2);
        expect(countries.taxRule('BE').applyTax(1)).toBe(1.24);
        expect(countries.taxRule('EL').applyTax(1)).toBe(1.2);
        expect(countries.taxRule('CZ').applyTax(1)).toBe(1.19);
        expect(countries.taxRule('PT').applyTax(1)).toBe(1.23);
        expect(countries.taxRule('HU').applyTax(1)).toBe(1.27);
        expect(countries.taxRule('SE').applyTax(1)).toBe(1.23);
        expect(countries.taxRule('AT').applyTax(1)).toBe(1.22);
        expect(countries.taxRule('BG').applyTax(1)).toBe(1.21);
        expect(countries.taxRule('DK').applyTax(1)).toBe(1.21);
        expect(countries.taxRule('FI').applyTax(1)).toBe(1.17);
        expect(countries.taxRule('SK').applyTax(1)).toBe(1.18);
        expect(countries.taxRule('IE').applyTax(1)).toBe(1.21);
        expect(countries.taxRule('HR').applyTax(1)).toBe(1.23);
        expect(countries.taxRule('LT').applyTax(1)).toBe(1.23);
        expect(countries.taxRule('SI').applyTax(1)).toBe(1.24);
        expect(countries.taxRule('LV').applyTax(1)).toBe(1.2);
        expect(countries.taxRule('EE').applyTax(1)).toBe(1.22);
        expect(countries.taxRule('CY').applyTax(1)).toBe(1.21);
        expect(countries.taxRule('LU').applyTax(1)).toBe(1.25);
        expect(countries.taxRule('MT').applyTax(1)).toBe(1.2);
    });

    it('should get the updated tax for a given country', function() {
    	var newTaxRule = function(total) { if(total > 100) return total * 1.2; else return total * 1.2 + 100; };
	    countries.updateTax('FR', newTaxRule);

    	var newTax = countries.taxRule('FR');

	    expect(newTax.applyTax(100)).toBe(100 * 1.2 + 100);
	    expect(newTax.applyTax(150)).toBe(150 * 1.2);
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

