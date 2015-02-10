'use strict';

var services = require('../public/javascripts/services');

describe('sellers', function(){
   	var sellers = services.sellers;

    it('should register the users\' URL', function() {
        sellers.register('url1');
        sellers.register('url2');
		
        expect(sellers.all).toContain("url1");
    });

    it('should send the request to all users', function(){
        var expectedCall = 0;
        var callback = function(p){
        	expectedCall++;
        };

        sellers.sendRequest(callback);
		expect(expectedCall).toEqual(2);
	});
});
