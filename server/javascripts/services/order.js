const repositories = require('../repositories')
const _ = require('lodash')
const utils = require('../utils')
const chalk = require('chalk')

function OrderService (configuration) {
  this.countries = new repositories.Countries(configuration)
}

module.exports = OrderService

const service = OrderService.prototype

service.sendOrder = function (seller, order, cashUpdater, logError) {
  console.info(chalk.grey('Sending order ' + utils.stringify(order) + ' to seller ' + utils.stringify(seller)))
  utils.post(seller.hostname, seller.port, seller.path + '/order', order, cashUpdater, logError)
}

service.createOrder = function (reduction) {
  const items = _.random(1, 10)
  const prices = new Array(items)
  const quantities = new Array(items)
  const country = this.countries.randomOne()

  for (let item = 0; item < items; item++) {
    const price = _.random(1, 100, true)
    prices[item] = utils.fixPrecision(price, 2)
    quantities[item] = _.random(1, 10)
  }

  return {
    prices,
    quantities,
    country,
    reduction: reduction.name
  }
}

service.bill = function (order, reduction) {
  const prices = order.prices
  const quantities = order.quantities
  let sum = quantities
    .map(function (q, i) { return q * prices[i] })
    .reduce(function (sum, current) { return sum + current }, 0)

  const taxRule = this.countries.taxRule(order.country)
  sum = taxRule.applyTax(sum)
  sum = reduction.apply(sum)
  return { total: sum }
}

service.validateBill = function (bill) {
  if (!_.has(bill, 'total')) {
    throw new Error('The field "total" in the response is missing.')
  }

  if (!_.isNumber(bill.total)) {
    throw new Error('"Total" is not a number.')
  }
}
