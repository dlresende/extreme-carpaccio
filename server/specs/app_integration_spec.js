'use strict';

var _ = require('lodash'),
    request = require('supertest'),
    express = require('express'),
    bodyParser = require('body-parser'),
    //
    routes = require('../javascripts/routes'),
    services = require('../javascripts/services'),
    repositories = require('../javascripts/repositories'),
    Configuration = require('../javascripts/config').Configuration;


describe('Route', function () {
    var sellers, dispatcher, sellerService, orderService, configuration;
    var app;

    var done, error, grabError = function (err, res) {
        if (err) {
            error = err;
        }
        done = true;
    }, isDone = function () {
        return done;
    };

    beforeEach(function () {
        configuration = new Configuration();
        sellers = new repositories.Sellers();
        sellerService = new services.SellerService(sellers);
        orderService = new services.OrderService(configuration);
        dispatcher = new services.Dispatcher(sellerService, orderService, configuration);

        app = express();
        app.use(bodyParser.json());
        app.use(bodyParser.urlencoded({ extended: false }));
        app.use('/', routes(sellerService, dispatcher));

        done = false;
        error = null;
    });

    it('should register new seller', function () {
        runs(function () {
            request(app)
                .post('/seller')
                .set('Content-Type', 'application/json')
                .send({name: 'john', password: 'doe', url: 'http://localhost:6000'})
                .expect(200)
                .end(grabError);

        });

        waitsFor(isDone, "route should be resolved", 1750);

        runs(function() {
           expect(error).toBeNull();
        });
    });

    it('should register existing seller with same password', function () {
        var travis = {name: 'john', password: 'doe'};
        sellers.save(travis);

        runs(function () {
            request(app)
                .post('/seller')
                .send({name: 'john', password: 'doe', url: 'http://localhost:6000'})
                .expect(200)
                .end(grabError);
        });

        waitsFor(isDone, "route should be resolved", 1750);


        runs(function() {
            expect(error).toBeNull();
        });
    });

    it('should not register existing seller with different password', function () {
        var travis = {name: 'john', password: 'doe'};
        sellers.save(travis);

        runs(function () {
            request(app)
                .post('/seller')
                .send({name: 'john', password: 'smith', url: 'http://localhost:6000'})
                .expect(401)
                .end(grabError);
        });

        waitsFor(isDone, "route should be resolved", 1750);

        runs(function() {
            expect(error).toBeNull();
        });
    });
});