"use strict";

casper.test.begin('/order : Managed data', function suite(test) {
    var managedOrders = [
        {
            order: {
                'prices[]': [15.99],
                'quantities[]': [1],
                'country': 'ES',
                'reduction': 'STANDARD'
            },
            expected: {
                value: 19.03
            }
        },
        {
            order: {
                'prices[]': [4.1,8.03,86.83,65.62,44.82],
                'quantities[]': [10,3,5,4,5],
                'country': 'AT',
                'reduction':'STANDARD'
            },
            expected: {
                value: 1166.62
            }
        }
    ];

    casper.each(managedOrders, function(self, orderData) {
        self.thenOpen('http://project.loc/order', {
            method: 'post',
            data: orderData.order
        }).then(function(response) {
            casper.test.assertHttpStatus(200);
            casper.test.assertMatch(
                response.headers.get('Content-Type'),
                new RegExp('|^application/json|', 'i'),
                'Content-Type match the expected value'
            );
            var content = JSON.parse(this.getPageContent());
            casper.test.assertEquals(content.total, orderData.expected.value, 'Order total computation equals the expected value');
        });
    }).run(function () {
        test.done();
    });
});
