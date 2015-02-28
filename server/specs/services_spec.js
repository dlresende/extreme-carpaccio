'use strict';

var _ = require('lodash');
var services = require('../javascripts/services');
var repositories = require('../javascripts/repositories');
var utils = require('../javascripts/utils');
var http = require('http');

var Dispatcher = services.Dispatcher;
var OrderService = services.OrderService;
var SellerService = services.SellerService;
var Countries = repositories.Countries;
var Sellers = repositories.Sellers;

(function disableLogs() {
    console.info = console.error = function() {};
})();

describe('Seller Service', function() {
    var sellers, sellerService, bob;

    beforeEach(function() {
        bob = {name: 'bob', hostname: 'localhost', port: '3000', path: '/path/', cash: 0};
        sellers = new Sellers();
        sellerService = new SellerService(sellers);
    });

    it('should register new seller', function() {
        sellerService.register('http://localhost:3000/path/', 'bob');

        expect(sellerService.all()).toContain(bob);
    });

    it('should compute seller\'s cash based on the order\'s amount', function() {
        var bob = {name: 'bob', cash: 0};
        sellers.add(bob);

        sellerService.updateCash(bob, {total: 100}, {total: 100});

        expect(sellerService.all()).toContain({name: 'bob', cash: 100})
    });

    it('should compare seller\'s response with expected one using precision 2', function() {
        var bob = {name: 'bob', cash: 0};
        sellers.add(bob);

        sellerService.updateCash(bob, {total: 100.12345}, {total: 100.12});

        expect(sellerService.all()).toContain({name: 'bob', cash: 100.12})
    });

    it('should send notification to seller', function() {
        spyOn(utils, 'post');
        var message = {type: 'info', content: 'test'};

        sellerService.notify(bob, message);

        expect(utils.post).toHaveBeenCalledWith('localhost', '3000', '/path/feedback', message);
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

        orderService.sendOrder({hostname: 'localhost', port: '3000', path: '/test'}, order, cashUpdater);

        expect(utils.post).toHaveBeenCalledWith('localhost', '3000', '/test', order, cashUpdater);
    });

    it('should create an order with N item prices', function() {
        var numberOfItems = 5;

        var order = orderService.createOrder(numberOfItems);

        expect(order.prices.length).toBe(numberOfItems);
        expect(_.every(order.prices, Number)).toBeTruthy();
    });

    it('should create an order with N item quantities', function() {
        var numberOfItems = 5;

        var order = orderService.createOrder(numberOfItems);

        expect(order.quantities.length).toBe(numberOfItems);
        expect(_.every(order.quantities, Number)).toBeTruthy();
    });

    it('should create orders with countries of Europe', function() {
        var order = orderService.createOrder();

        expect(countries.fromEurope).toContain(order.country);
    });

    it('should calculate the sum of the order', function() {
        var order = {prices: [100, 50], quantities: [1, 2], country: 'IT'};

        var bill = orderService.bill(order);

        expect(bill).toEqual({total: (100 + 2 * 50) * 1.2});
    });

    it('should calculate the sum of the order with reduction', function() {
        var order = {prices: [100, 10], quantities: [10, 50], country: 'IT'};

        var bill = orderService.bill(order);

        expect(bill).toEqual({total: 1746});
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
    var sellers;
    var orderService;

    beforeEach(function(){
        sellers = new Sellers();
        orderService = new OrderService();
        dispatcher = new Dispatcher(sellers, orderService);
    });

    it('should send the same order to each seller', function() {
        var alice = {name: 'alice', hostname : 'seller', port : '8080', path : '/', cash: 0};
        sellers.add(alice);
        var bob = {name: 'bob', hostname : 'seller', port : '8081', path : '/', cash: 0};
        sellers.add(bob);
        var order = {prices: [100, 50], quantities: [1, 2], country: 'IT'};
        spyOn(orderService, 'createOrder').andReturn(order);
        spyOn(orderService, 'sendOrder');

        dispatcher.sendOrderToSellers();

        expect(orderService.createOrder).toHaveBeenCalled();
        expect(orderService.sendOrder).toHaveBeenCalledWith(alice, order, jasmine.any(Function));
        expect(orderService.sendOrder).toHaveBeenCalledWith(bob, order, jasmine.any(Function));
    });
});