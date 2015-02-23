'use strict';

var utils = require('../javascripts/utils');

describe('Utils', function() {

    it('should fix precision for numbers', function() {
        expect(utils.fixPrecision(1, 2)).toBe(1.00);
        expect(utils.fixPrecision(1.234, 2)).toBe(1.23);
    });

    it('should not fix precision and return 0.0 when input is not a number', function() {
        expect(utils.fixPrecision('1.234', 2)).toBe(0.00);
    });

    it('should not fix precision and return 0.0 when input is null or undefined', function() {
        expect(utils.fixPrecision(undefined, 2)).toBe(0.00);
        expect(utils.fixPrecision(null, 2)).toBe(0.00);
    });

    it('should parse a string into a json object', function() {
        expect(utils.jsonify('{"name": "Bob"}')).toEqual({name: 'Bob'})
    });

    it('should not parse a string but return an empty object when input is not a valid json object', function() {
        expect(utils.jsonify('not valid json object')).toEqual({});
        expect(utils.jsonify('')).toEqual({});
        expect(utils.jsonify(undefined)).toEqual({});
    });
});