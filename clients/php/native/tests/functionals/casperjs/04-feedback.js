"use strict";

casper.test.begin('/feedback : Method not allowed', function suite(test) {
    casper.start('http://project.loc/feedback', function(response) {
        casper.test.assertHttpStatus(405);
        casper.test.assertMatch(
            response.headers.get('Content-Type'),
            new RegExp('|^application/json|', 'i'),
            'Content-Type match the expected value'
        );
    });
    casper.then(function() {
        var content = JSON.parse(this.getPageContent());
        casper.test.assertEquals(content.error, 'Method not allowed; You must use POST', "Method not allowed");
    });
    casper.run(function () {
        test.done();
    });
});

