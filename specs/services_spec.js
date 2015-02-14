'use strict';

var http = require('http');
var _ = require('lodash');
var OrderService = require('../javascripts/services').OrderService;
var Countries = require('../javascripts/repositories').Countries;

describe('sellers', function(){

    var orderService;
    var countries;

    beforeEach(function(){
        orderService = new OrderService(http);
        countries = new Countries();
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

    it('should send orders to registered sellers', function() {
        //OrderService.startPurchasing();
    });
});