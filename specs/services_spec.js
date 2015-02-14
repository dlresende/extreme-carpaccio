'use strict';

var _ = require('lodash');
var services = require('../javascripts/services');
var repositories = require('../javascripts/repositories');

describe('Order Service', function() {

    var http = require('http');
    var orderService;
    var countries;

    beforeEach(function(){
        orderService = new services.OrderService(http);
        countries = new repositories.Countries();
    });

    it('should send order to seller with request', function() {
        var fakeRequest = {
            write: function() {},
            end: function() {}
        };
        spyOn(http, 'request').andReturn(fakeRequest);
        spyOn(fakeRequest, 'write');
        spyOn(fakeRequest, 'end');
        var order ={
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
        expect(fakeRequest.write).toHaveBeenCalledWith(order);
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
        sellers = new repositories.Sellers();
        orderService = new services.OrderService();
        dispatcher = new services.Dispatcher(sellers, orderService);
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