var _ = require('lodash')
var Reduction = require('./reduction')
var utils = require('../utils')
var chalk = require('chalk')

var BadRequest = function (_configuration) {
  this.configuration = _configuration
}

BadRequest.prototype = (function () {
  function getConfiguration (self) {
    return self.configuration.all().badRequest
  }

  return {
    shouldSendBadRequest: function (iteration) {
      return getConfiguration(this).active && (iteration % getConfiguration(this).period === 0)
    },

    corruptOrder: function (order) {
      var mode = _.sample(getConfiguration(this).modes)
      var copy = _.clone(order)

      console.info(chalk.blue('corrupt mode ' + mode))

      switch (mode) {
        case 0:
          return {}
        case 1:
          return _.map(_.range(17), function (i) {
            return i % 2 === 0
          })
        case 2:
          copy.quantities = { error: 'datacenter unreachable' }
          return copy
        case 3:
          copy.quantities = copy.quantities.slice(1)
          return copy
        case 4:
          copy.prices = copy.prices.slice(1)
          return copy
        case 5:
          copy.country = 'Llanfairpwllgwyngyllgogerychwyrndrobwllllantysiliogogogoch'
          return copy
        case 6:
          delete copy.country
          return copy
        case 7:
          delete copy.prices
          return copy
        case 8:
          delete copy.quantities
          return copy
        case 9:
          delete copy.reduction
          return copy
        case 10:
          return null
      }
      return {}
    },

    updateSellersCash: function (self, seller, expectedBill, currentIteration) {
      return function (response) {
        var amount = expectedBill.total
        var sellerService = self.sellerService
        var message

        if (response.statusCode !== 400) {
          var loss = amount * 0.5
          message = 'Hey, ' + seller.name + ' lose ' + loss + ' because he/she does not know how to handle correctly a bad request'
          sellerService.deductCash(seller, loss, currentIteration)
          sellerService.notify(seller, { type: 'ERROR', content: message })
        } else {
          message = 'Hey, ' + seller.name + ' earned ' + amount
          sellerService.addCash(seller, amount, currentIteration)
          sellerService.notify(seller, { type: 'INFO', content: message })
        }
      }
    }
  }
})()

var SellerCashUpdater = function (_sellerService, _orderService) {
  this.sellerService = _sellerService
  this.orderService = _orderService
}

SellerCashUpdater.prototype = (function () {
  return {
    doUpdate: function (seller, expectedBill, currentIteration) {
      var self = this
      return function (response) {
        if (response.statusCode === 200) {
          self.sellerService.setOnline(seller)

          response.on('error', function (err) {
            console.error(err)
          })

          response.on('data', function (sellerResponse) {
            console.info(chalk.grey(seller.name + ' replied "' + sellerResponse + '"'))

            try {
              var actualBill = utils.jsonify(sellerResponse)
              self.orderService.validateBill(actualBill)
              self.sellerService.updateCash(seller, expectedBill, actualBill, currentIteration)
            } catch (exception) {
              self.sellerService.notify(seller, { type: 'ERROR', content: exception.message })
            }
          })
        } else if (response.statusCode === 404) {
          self.sellerService.setOnline(seller)
          console.info(chalk.grey(seller.name + ' replied 404. Everything is fine.'))
        } else {
          self.sellerService.setOnline(seller)
          self.sellerService.updateCash(seller, expectedBill, undefined, currentIteration)
        }
      }
    }
  }
})()

var Dispatcher = function (_sellerService, _orderService, _configuration) {
  this.sellerService = _sellerService
  this.orderService = _orderService
  this.configuration = _configuration
  this.offlinePenalty = 0
  this.badRequest = new BadRequest(_configuration)
  this.sellerCashUpdater = new SellerCashUpdater(_sellerService, _orderService)
}

Dispatcher.prototype = (function () {
  function putSellerOffline (self, seller, currentIteration) {
    return function () {
      console.error(chalk.red('Could not reach seller ' + utils.stringify(seller)))
      var offlinePenalty = getConfiguration(self).offlinePenalty

      if (!_.isNumber(offlinePenalty)) {
        console.warn(chalk.yellow('Offline penalty is missing or is not a number. Using 0.'))
        offlinePenalty = 0
      }

      self.sellerService.setOffline(seller, offlinePenalty, currentIteration)
    }
  }

  var Period = function (reduction, shoppingIntervalInMillis) {
    this.reduction = reduction
    this.shoppingIntervalInMillis = shoppingIntervalInMillis
  }

  function getReductionPeriodFor (reductionStrategy) {
    if (reductionStrategy === 'PAY THE PRICE') {
      return new Period(Reduction.PAY_THE_PRICE, 10000)
    }

    if (reductionStrategy === 'HALF PRICE') {
      return new Period(Reduction.HALF_PRICE, 1000)
    }

    if (reductionStrategy !== 'STANDARD') {
      console.warn(chalk.yellow('Unknown reduction strategy ' + reductionStrategy + '. Using STANDARD.'))
    }

    return new Period(Reduction.STANDARD, 5000)
  }

  function scheduleNextIteration (self, nextIteration, intervalInMillis) {
    setTimeout(function () {
      self.startBuying(nextIteration)
    }, intervalInMillis)
  }

  function shouldSendOrders (self) {
    const active = getConfiguration(self).active
    return active == null ? true : active
  }

  function getConfiguration (self) {
    return self.configuration.all()
  }

  return {
    sendOrderToSellers: function (reduction, currentIteration, badRequest) {
      var self = this
      var order = self.orderService.createOrder(reduction)
      var expectedBill = self.orderService.bill(order, reduction)

      if (badRequest) {
        order = self.badRequest.corruptOrder(order)
      }

      _.forEach(self.sellerService.allSellers(), function (seller) {
        self.sellerService.addCash(seller, 0, currentIteration)
        var cashUpdater

        if (badRequest) {
          cashUpdater = self.badRequest.updateSellersCash(self, seller, expectedBill, currentIteration)
        } else {
          cashUpdater = self.sellerCashUpdater.doUpdate(seller, expectedBill, currentIteration)
        }

        var errorCallback = putSellerOffline(self, seller, currentIteration)
        self.orderService.sendOrder(seller, order, cashUpdater, errorCallback)
      })
    },

    startBuying: function (iteration) {
      var reductionStrategy = getConfiguration(this).reduction
      var period = getReductionPeriodFor(reductionStrategy)
      var badRequest = this.badRequest.shouldSendBadRequest(iteration)
      var message = '>>> Shopping iteration ' + iteration
      var nextIteration = iteration + 1

      if (badRequest) {
        message = message + ' (bad request)'
      }

      if (shouldSendOrders(this)) {
        console.info(chalk.green(message))
        this.sendOrderToSellers(period.reduction, iteration, badRequest)
      } else {
        nextIteration = iteration
        console.info(chalk.red('Order dispatching disabled'))
      }

      scheduleNextIteration(this, nextIteration, period.shoppingIntervalInMillis)
      return nextIteration
    }
  }
})()

module.exports = {
  Dispatcher: Dispatcher,
  BadRequest: BadRequest,
  SellerCashUpdater: SellerCashUpdater
}
