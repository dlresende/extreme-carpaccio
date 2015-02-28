var express = require('express');
var path = require('path');
var favicon = require('serve-favicon');
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

app.set('views', __dirname);
app.set('view engine', 'ejs');

// uncomment after placing your favicon in /public
//app.use(favicon(__dirname + '/public/favicon.ico'));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: false }));
app.use(cookieParser());
app.use(express.static(path.join(__dirname, 'public')));

app.use('/', routes);

dispatcher.startBuying(5000);

module.exports = app;
