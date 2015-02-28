'use strict';

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
});