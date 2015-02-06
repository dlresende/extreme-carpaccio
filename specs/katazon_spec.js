'use strict';

var server = require('../routes/server.js');

describe('matching underscore', function(){
    it('should save the user URL', function() {
        server.register('url');

        expect(server.sellers).toContain("url");
    });
});