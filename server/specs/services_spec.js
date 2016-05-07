'use strict';

var _ = require('lodash');
var services = require('../javascripts/services');
var repositories = require('../javascripts/repositories');
var utils = require('../javascripts/utils');
var http = require('http');

var Dispatcher = services.Dispatcher;
var OrderService = services.OrderService;
var SellerService = services.SellerService;
var Reduction = services.Reduction;
var Countries = repositories.Countries;
var Sellers = repositories.Sellers;
var UrlAssembler = require('url-assembler');

(function disableLogs() {
    console.info = console.error = function() {};
})();

describe('Seller Service', function() {
    var sellers, sellerService, bob;

    beforeEach(function() {
        bob = {name: 'bob', hostname: 'localhost', port: '3000', path: '/path', cash: 0, online: false};
        sellers = new Sellers();
        sellerService = new SellerService(sellers);
    });

    it('should register new seller', function() {
        sellerService.register('http://localhost:3000/path', 'bob');
        var sellers = sellerService.allSellers();
        expect(sellers.length).toBe(1);
        var actual = sellers.shift()
        expect(actual.name).toBe('bob');
        expect(actual.cash).toBe(0);
        expect(actual.online).toBe(false);
        expect(actual.url instanceof UrlAssembler).toBeTruthy();
        expect(actual.url.toString()).toBe('http://localhost:3000/path')
    });

    it('should compute seller\'s cash based on the order\'s amount', function() {
        var bob = {name: 'bob', cash: 0};
        sellers.save(bob);

        sellerService.updateCash(bob, {total: 100}, {total: 100});

        expect(sellerService.allSellers()).toContain({name: 'bob', cash: 100})
    });

    it('should deduct 50% of the bill amount from seller\'s cash when the seller\'s bill does not correspond with the expected one', function() {
        var bob = {name: 'bob', cash: 0};
        sellers.save(bob);

        sellerService.updateCash(bob, {total: 100}, {total: 50});

        expect(sellerService.allSellers()).toContain({name: 'bob', cash: -50})
    });

    it('should deduct a penalty when a seller is offline', function(){
        var bob = {name: 'bob', cash: 200, online: true};
        var offlinePenalty = 100;
        sellers.save(bob);

        sellerService.setOffline(bob, offlinePenalty);

        expect(bob.online).toBe(false);
        expect(bob.cash).toBe(100);
    });

    it('should compare seller\'s response with expected one using precision 2', function() {
        var bob = {name: 'bob', cash: 0};
        sellers.save(bob);

        sellerService.updateCash(bob, {total: 100.12345}, {total: 100.12});

        expect(sellerService.allSellers()).toContain({name: 'bob', cash: 100.12})
    });

    it('should send notification to seller', function() {
        spyOn(utils, 'post');
        var message = {type: 'info', content: 'test'};

        sellerService.notify(bob, message);

        expect(utils.post).toHaveBeenCalledWith('localhost', '3000', '/path/feedback', message);
    });

    it('should get seller\'s cash history reduced in chunks of N iterations', function() {
        sellers.cashHistory = {'bob': [0, 0, 10, 10, 10]};

        var cashHistory = sellerService.getCashHistory(5);

        expect(cashHistory).toEqual({history: {'bob': [10]}, lastIteration: 5});
    });

    it('should get seller\'s cash history reduced in chunks of N iterations and add remaining iterations when last chunk is not completed', function() {
        sellers.cashHistory = {'bob': [0, 0, 10, 10, 10, 10, 10]};

        var cashHistory = sellerService.getCashHistory(3);

        expect(cashHistory).toEqual({history: {'bob': [10, 10, 10]}, lastIteration: 7});
    });

    it('should authorized unknown seller', function() {
        expect(sellerService.isAuthorized('carmen', 'mccallum')).toEqual(true);
    });

    it('should authorized seller if the same username and password are provided', function() {
        var travis = {name: 'travis', password:'pacman'};
        sellers.save(travis);

        expect(sellerService.isAuthorized('travis', 'pacman')).toEqual(true);
        expect(sellerService.isAuthorized('travis', 'vlad')).toEqual(false);
    });
});

describe('Order Service', function() {
    var orderService;
    var countries;

    beforeEach(function(){
        orderService = new OrderService();
        countries = new Countries();
    });

    it('should send order to seller', function() {
        spyOn(utils, 'post');
        var order = {
            quantity: [1, 2, 3],
            prices: [12.1, 10, 11],
            state: "CA"
        };
        var cashUpdater = function() {};
        var onError = function() {};

        orderService.sendOrder({hostname: 'localhost', port: '3000', path: '/test'}, order, cashUpdater, onError);

        expect(utils.post).toHaveBeenCalledWith('localhost', '3000', '/test/order', order, cashUpdater, onError);
    });

    it('should create an order with maximum 10 items', function() {
        var order = orderService.createOrder(Reduction.STANDARD);

        expect(order.prices.length).toBeGreaterThan(0);
        expect(order.prices.length).not.toBeGreaterThan(10);
        expect(_.every(order.prices, Number)).toBeTruthy();
        expect(order.quantities.length).toBeGreaterThan(0);
        expect(order.quantities.length).not.toBeGreaterThan(10);
        expect(_.every(order.quantities, Number)).toBeTruthy()
    });

    it('should create orders with countries of Europe', function() {
        var order = orderService.createOrder(Reduction.STANDARD);

        expect(countries.fromEurope).toContain(order.country);
    });

    it('should create orders using specific reduction type', function() {
        expect(orderService.createOrder(Reduction.STANDARD).reduction).toContain(Reduction.STANDARD.name);
        expect(orderService.createOrder(Reduction.PAY_THE_PRICE).reduction).toContain(Reduction.PAY_THE_PRICE.name);
        expect(orderService.createOrder(Reduction.HALF_PRICE).reduction).toContain(Reduction.HALF_PRICE.name);
    });

    it('should calculate the sum of the order using PAY_THE_PRICE reduction', function() {
        var order = {prices: [1000, 50], quantities: [1, 2], country: 'IT'};

        var bill = orderService.bill(order, Reduction.PAY_THE_PRICE);

        expect(bill).toEqual({total: (1000 + 2 * 50) * countries.tax('IT')});
    });

    it('should calculate the sum of the order using STANDARD reduction', function() {
        var order = {prices: [1000, 50], quantities: [1, 2], country: 'IT'};

        var bill = orderService.bill(order, Reduction.STANDARD);

        expect(bill).toEqual({total: (1000 + 2 * 50) * countries.tax('IT') * (1 - 0.03)});
    });

    it('should calculate the sum of the order using HALF_PRICE reduction', function() {
        var order = {prices: [1000, 50], quantities: [1, 2], country: 'IT'};

        var bill = orderService.bill(order, Reduction.HALF_PRICE);

        expect(bill).toEqual({total: (1000 + 2 * 50) * countries.tax('IT') / 2});
    });

    it('should not validate bill when total field is missing', function() {
        expect(function(){orderService.validateBill({})}).toThrow('The field \"total\" in the response is missing.');
    });

    it('should not validate bill when total is not a number', function() {
        expect(function(){orderService.validateBill({total: 'NaN'})}).toThrow('\"Total\" is not a number.');
    });
});

describe('Dispatcher', function() {
    var dispatcher;
    var orderService;
    var sellerService;

    beforeEach(function(){
        sellerService = new SellerService();
        orderService = new OrderService();
        dispatcher = new Dispatcher(sellerService, orderService);
    });

    it('should send the same order to each seller using reduction', function() {
        var alice = {name: 'alice', hostname : 'seller', port : '8080', path : '/', cash: 0};
        var bob = {name: 'bob', hostname : 'seller', port : '8081', path : '/', cash: 0};
        spyOn(sellerService, 'addCash');
        spyOn(sellerService, 'allSellers').andReturn([alice, bob]);
        var order = {prices: [100, 50], quantities: [1, 2], country: 'IT'};
        spyOn(orderService, 'createOrder').andReturn(order);
        spyOn(orderService, 'sendOrder');

        dispatcher.sendOrderToSellers(Reduction.STANDARD);

        expect(orderService.createOrder).toHaveBeenCalledWith(Reduction.STANDARD);
        expect(orderService.sendOrder).toHaveBeenCalledWith(alice, order, jasmine.any(Function), jasmine.any(Function));
        expect(orderService.sendOrder).toHaveBeenCalledWith(bob, order, jasmine.any(Function), jasmine.any(Function));
    });
});

describe('Standard Reduction', function() {
    var standardReduction;

    beforeEach(function() {
        standardReduction = Reduction.STANDARD;
    });

    it('should de reduced by 15% when total is bigger than 50,000', function() {
        expect(standardReduction.reductionFor(50001)).toBe(0.15);
    });

    it('should de reduced by 10% when total is between [10,000, 50,000)', function() {
        expect(standardReduction.reductionFor(10000)).toBe(0.10);
        expect(standardReduction.reductionFor(10500)).toBe(0.10);
    });

    it('should de reduced by 7% when total is between [7,000, 10,000)', function() {
        expect(standardReduction.reductionFor(7000)).toBe(0.07);
        expect(standardReduction.reductionFor(7500)).toBe(0.07);
    });

    it('should be reduced by 5% when total is between [5,000, 7,000)', function() {
        expect(standardReduction.reductionFor(5000)).toBe(0.05);
        expect(standardReduction.reductionFor(5500)).toBe(0.05);
    });

    it('should be reduced by 3% when total is between [1,000, 5,000)', function() {
        expect(standardReduction.reductionFor(1000)).toBe(0.03);
        expect(standardReduction.reductionFor(1100)).toBe(0.03);
    });

    it('should not be reduced when when the total between [0, 1,000)', function() {
        expect(standardReduction.reductionFor(500)).toBe(0.00);
    });
});
