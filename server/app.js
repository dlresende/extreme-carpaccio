var express = require('express');
var path = require('path');
var favicon = require('serve-favicon');
var logger = require('morgan');
var cookieParser = require('cookie-parser');
var bodyParser = require('body-parser');

var services = require('./javascripts/services');
var repositories = require('./javascripts/repositories');

var sellers = new repositories.Sellers();

var sellerService = new services.SellerService(sellers);
var orderService = new services.OrderService();
var dispatcher = new services.Dispatcher(sellerService, orderService);

var routes = require('./javascripts/routes')(sellerService);

var app = express();

// uncomment after placing your favicon in /public
//app.use(favicon(__dirname + '/public/favicon.ico'));
app.use(logger('dev'));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: false }));
app.use(cookieParser());
app.use(express.static(path.join(__dirname, 'public')));

app.use('/', routes);

// catch 404 and forward to error handler
app.use(function(req, res, next) {
    var err = new Error('Not Found');
    err.status = 404;
    next(err);
});

// error handlers

// development error handler
// will print stacktrace
if (app.get('env') === 'development') {
    app.use(function(err, req, res, next) {
        res.status(err.status || 500);
        res.render('error', {
            message: err.message,
            error: err
        });
    });
}

// production error handler
// no stacktraces leaked to user
app.use(function(err, req, res, next) {
    res.render('public/index.html');
    res.status(err.status || 500);
    res.render('error', {
        message: err.message,
        error: {}
    });
});

dispatcher.startBuying(5000);

module.exports = app;
