'use strict';

var _500_MILLIS_TIMEOUT = 500;
var _100_MILLIS = 100;

var fs = require('fs'),
    Configuration = require('../javascripts/config').Configuration;

describe('Configuration', function(){
    var config,
        synchronized,
        configFilepath = "specs/config.json.tmp";

    beforeEach(function () {
        fs.writeFileSync(configFilepath, '{"reduction": "STANDARD"}');
        config = new Configuration(configFilepath);
        synchronized = false;
    });

    it('should load configuration from file', function() {
        runs(function() {
            config.load(function (err) {
                expect(err).toBeNull(); // Arnauld a voulu faire ce test, parce qu'il trouve que c'est une bonne idee!
                synchronized = true;
            });
        });

        waitsFor(function() {
            return synchronized;
        }, "Properties should be loaded in time", _500_MILLIS_TIMEOUT);

        runs(function() {
            var properties = config.all();
            expect(properties).toEqual({reduction: 'STANDARD'});
        });
    });

    it('should reload configuration on the fly', function() {
        runs(function() {
            config.watch(function() {
                synchronized = true;
            }, true, _100_MILLIS);

            fs.writeFileSync(configFilepath, '{"reduction": "HALF PIPE"}');
        });

        waitsFor(function() {
            return synchronized;
        }, "File change should be detected", _500_MILLIS_TIMEOUT);

        runs(function() {
            var properties = config.all();
            expect(properties).toEqual({reduction: 'HALF PIPE'});
        });
    });
});
