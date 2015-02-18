'use strict';

var _ = require('lodash');
var services = require('../javascripts/services');
var repositories = require('../javascripts/repositories');
var utils = require('../javascripts/utils');

var Dispatcher = services.Dispatcher;
var OrderService = services.orderService;
var SellerService = services.SellerService;
var Countries = repositories.Countries;
var Sellers = repositories.Sellers;

describe('Seller Service', function() {
    var sellers;
    var sellerService;

    beforeEach(function() {
        sellers = new Sellers();
        sellerService = new SellerService(sellers);
    });

    it('should register new seller', function() {
        sellerService.register('http://localhost:3000/path', 'bob');

        expect(sellerService.all()).toContain({name: 'bob', hostname: 'localhost', port: '3000', path: '/path', cash: 0});
    });

    it('should compute seller\'s cash based on the order\'s amount', function() {
        var bob = {name: 'bob', cash: 0};
        sellers.add(bob);

        sellerService.updateCash('bob', {total: 100}, {total: 100});

        expect(sellerService.all()).toContain({name: 'bob', cash: 100})
    });

    it('should float comparaison be based on rounded number', function() {
        var bob = {name: 'bob', cash: 0};
        sellers.add(bob);

        sellerService.updateCash('bob', {total: 100.12345}, {total: 100.12});

        expect(sellerService.all()).toContain({name: 'bob', cash: 100.12})
    });
});

describe('Order Service', function() {

    var http = require('http');
    var orderService;
    var countries;

    beforeEach(function(){
        orderService = new OrderService(http);
        countries = new Countries();
    });

    it('should send order to seller', function() {
        var fakeRequest = {
            write: function() {},
            end: function() {}
        };
        spyOn(http, 'request').andReturn(fakeRequest);
        spyOn(fakeRequest, 'write');
        spyOn(fakeRequest, 'end');
        var order = {
            quantity: [1, 2, 3],
            prices: [12.1, 10, 11],
            state: "CA"
        };
        var cashUpdater = function() {};

        orderService.sendOrder({hostname: 'localhost', port: 3000, path: '/test'}, order, cashUpdater);

        expect(http.request).toHaveBeenCalledWith({
            hostname : 'localhost',
            port : 3000,
            path : '/test',
            method : 'POST',
            headers : {
                'Content-Type' : 'application/json'
            }
        }, cashUpdater);
        expect(fakeRequest.write).toHaveBeenCalledWith(utils.stringify(order));
        expect(fakeRequest.end).toHaveBeenCalled();
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