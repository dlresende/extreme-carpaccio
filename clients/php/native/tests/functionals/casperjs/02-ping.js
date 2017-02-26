"use strict";

casper.test.begin('Check http://project.loc/ping', 2, function(test) {
    casper.start('http://project.loc/ping', function() {
        test.assertHttpStatus(200);
    }).then(function() {
        test.assertEquals(this.getPageContent(), 'pong', "Pong found");
    }).run(function() {
        test.done();
    });
});
