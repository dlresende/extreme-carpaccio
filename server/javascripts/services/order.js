var repositories = require('../repositories');
var countries = new repositories.Countries();
var _ = require('lodash');
var utils = require('../utils');

module.exports = OrderService;

function OrderService () {}

var service = OrderService.prototype;

service.sendOrder = function (seller, order, cashUpdater, logError) {
  console.info('Sending order ' + utils.stringify(order) + ' to seller ' + utils.stringify(seller));
  utils.post(seller.hostname, seller.port, seller.path + '/order', order, cashUpdater, logError);
};

service.createOrder = function (reduction) {
  var items = _.random(1, 10);
  var prices = new Array(items);
  var quantities = new Array(items);

  for(var item = 0; item < items; item++) {
    var price = _.random(1, 100, true);
    prices[item] = utils.fixPrecision(price, 2);
    quantities[item] = _.random(1, 10);
  }

  return {
    prices: prices,
    quantities: quantities,
    country: countries.randomOne(),
    reduction: reduction.name
  };
};

service.bill = function (order, reduction) {
  var prices = order.prices;
  var quantities = order.quantities;
  var sum = quantities
    .map(function(q, i) {return q * prices[i]})
    .reduce(function(sum, current) {return sum + current}, 0);

  var taxRule = countries.taxRule(order.country);
  sum = taxRule.applyTax(sum);
  sum = reduction.apply(sum);
  return { total: sum };
};

service.validateBill = function (bill) {
  if(!_.has(bill, 'total')) {
    throw {message: 'The field \"total\" in the response is missing.'};
  }

  if(!_.isNumber(bill.total)) {
    throw {message: '\"Total\" is not a number.'};
  }
}
