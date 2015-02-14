'use strict';

var Dispatcher = require('../javascripts/services').Dispatcher;
var http = require('http');

describe('sellers', function(){

    var dispatcher;

    beforeEach(function(){
        dispatcher = new Dispatcher(http);
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

        dispatcher.sendOrder({hostname: 'localhost', port: 3000, path: '/test'}, order);

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
});