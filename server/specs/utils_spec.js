'use strict';

var http = require('http');

var utils = require('../javascripts/utils');

describe('Utils', function() {

    it('should fix precision for numbers', function() {
        expect(utils.fixPrecision(1, 2)).toBe(1.00);
        expect(utils.fixPrecision(1.234, 2)).toBe(1.23);
    });

    it('should parse a string into a json object', function() {
        expect(utils.jsonify('{"name": "Bob"}')).toEqual({name: 'Bob'})
    });

    it('should not parse a string but return an empty object when input is not a valid json object', function() {
        expect(function(){utils.jsonify('not valid json object')}).toThrow();
        expect(function(){utils.jsonify('')}).toThrow();
        expect(function(){utils.jsonify(undefined)}).toThrow();
    });

    it('should send post request to someone else', function() {
        var fakeRequest = {
            write: function() {},
            on: function() {},
            end: function() {}
        };
        spyOn(http, 'request').andReturn(fakeRequest);
        spyOn(fakeRequest, 'write');
        spyOn(fakeRequest, 'end');
        var body = {content: 'some content'};
        var callback = function() {};

        utils.post('localhost', '3000', '/path', body, callback);

        var bodyStringified = utils.stringify(body);
        expect(http.request).toHaveBeenCalledWith({
            hostname : 'localhost',
            port : '3000',
            path : '/path',
            method : 'POST',
            headers : {
                'Content-Type' : 'application/json',
                'Accept' : 'application/json',
                'Content-Length' : bodyStringified.length
            }
        }, callback);
        expect(fakeRequest.write).toHaveBeenCalledWith(bodyStringified);
        expect(fakeRequest.end).toHaveBeenCalled();
    });
});