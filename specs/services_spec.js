'use strict';

var _ = require('lodash');
var services = require('../javascripts/services');
var repositories = require('../javascripts/repositories');
var utils = require('../javascripts/utils');

var Dispatcher = services.Dispatcher;
var OrderService = services.OrderService;
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
        sellerService.register('http://localhost:3000/path');

        expect(sellerService.all()).toContain({hostname: 'localhost', port: '3000', path: '/path'});
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

        orderService.sendOrder({hostname: 'localhost', port: 3000, path: '/test'}, order);

        expect(http.request).toHaveBeenCalledWith({
            hostname : 'localhost',
            port : 3000,
            path : '/test',
            method : 'POST',
            headers : {
                'Content-Type' : 'application/json'
            }
        });
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
        var seller = { hostname : 'seller', port : '8080', path : '/' };
        sellers.add(seller);
        var fakeOrder = {};
        spyOn(orderService, 'createOrder').andReturn(fakeOrder);
        spyOn(orderService, 'sendOrder');

        dispatcher.sendOrderToSellers();

        expect(orderService.createOrder).toHaveBeenCalled();
        expect(orderService.sendOrder).toHaveBeenCalledWith(seller, fakeOrder);
    });
});