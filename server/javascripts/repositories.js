'use strict'

const _ = require('lodash')
const chalk = require('chalk')

const Sellers = function () {
  const sellersMap = {}
  const cashHistory = {}

  this.cashHistory = cashHistory

  this.all = function () {
    const sellers = _.map(sellersMap, function (seller) {
      return seller
    })
    return _.sortBy(sellers, function (seller) { return -seller.cash })
  }

  this.save = function (seller) {
    if (sellersMap[seller.name] === undefined) {
      add(seller)
    } else {
      update(seller)
    }
  }

  this.get = function (sellerName) {
    return sellersMap[sellerName]
  }

  function add (seller) {
    sellersMap[seller.name] = seller
    cashHistory[seller.name] = []
  }
  function update (seller) {
    const previousCash = sellersMap[seller.name].cash
    sellersMap[seller.name] = seller
    sellersMap[seller.name].cash = previousCash
  }
}

Sellers.prototype = (function () {
  function getLastRecordedCashAmount (currentSellersCashHistory, lastRecordedIteration) {
    let lastRecordedValue = currentSellersCashHistory[lastRecordedIteration - 1]

    if (lastRecordedValue === undefined) {
      lastRecordedValue = 0
    }

    return lastRecordedValue
  }

  function enlargeHistory (newSize, oldHistory) {
    const newHistory = new Array(newSize)
    newHistory.push.apply(newHistory, oldHistory)
    return newHistory
  }

  function fillMissingIterations (currentIteration, currentSellersCashHistory) {
    const lastRecordedIteration = currentSellersCashHistory.length

    if (lastRecordedIteration >= currentIteration) {
      return currentSellersCashHistory
    }

    const newSellersCashHistory = enlargeHistory(currentIteration, currentSellersCashHistory)
    const lastRecordedValue = getLastRecordedCashAmount(currentSellersCashHistory, lastRecordedIteration)
    return _.fill(newSellersCashHistory, lastRecordedValue, lastRecordedIteration, currentIteration)
  }

  function updateCashHistory (self, seller, currentIteration) {
    const currentSellersCashHistory = self.cashHistory[seller.name]
    const newSellersCashHistory = fillMissingIterations(currentIteration, currentSellersCashHistory)
    newSellersCashHistory[currentIteration] = seller.cash
    self.cashHistory[seller.name] = newSellersCashHistory
  }

  return {
    count: function () {
      return this.all().length
    },

    isEmpty: function () {
      return this.count() === 0
    },

    updateCash: function (sellerName, amount, currentIteration) {
      const seller = this.get(sellerName)
      seller.cash += parseFloat(amount)
      updateCashHistory(this, seller, currentIteration)
    },

    setOffline: function (sellerName) {
      this.get(sellerName).online = false
    },

    setOnline: function (sellerName) {
      this.get(sellerName).online = true
    }
  }
})()

const Countries = function (configuration) {
  this.configuration = configuration
}

Countries.prototype = (function () {
  const europeanCountries = {
    DE: [1.2, 190995],
    UK: [1.21, 152741],
    FR: [1.2, 151381],
    IT: [1.25, 143550],
    ES: [1.19, 109023],
    PL: [1.21, 90574],
    RO: [1.2, 46640],
    NL: [1.2, 39842],
    BE: [1.24, 26510],
    EL: [1.2, 25338],
    CZ: [1.19, 24755],
    PT: [1.23, 24261],
    HU: [1.27, 23141],
    SE: [1.23, 23047],
    AT: [1.22, 20254],
    BG: [1.21, 16905],
    DK: [1.21, 13348],
    FI: [1.17, 12903],
    SK: [1.18, 12767],
    IE: [1.21, 10894],
    HR: [1.23, 9952],
    LT: [1.23, 6844],
    SI: [1.24, 4858],
    LV: [1.2, 4656],
    EE: [1.22, 3094],
    CY: [1.21, 2],
    LU: [1.25, 1],
    MT: [1.2, 1]
  }

  function scale (factor) {
    return function (price) { return price * factor }
  }

  function defaultTaxRule (name) {
    return scale(europeanCountries[name][0])
  }

  const Country = function (name, taxRule) {
    this.name = name
    this.taxRule = taxRule
  }

  function customEval (s) {
    return new Function('return ' + s)() // eslint-disable-line no-new-func
  }

  function lookupForOverridenDefinition (configuration, country) {
    const conf = configuration.all()
    if (!conf.taxes || !conf.taxes[country]) {
      return null
    }

    const def = conf.taxes[country]
    if (_.isNumber(def)) {
      console.info(chalk.blue('Tax rule for country ' + country + ' changed to scale factor ' + def))
      return scale(def)
    }

    if (_.isString(def)) {
      try {
        const taxRule = customEval(def)
        if (_.isFunction(taxRule)) {
          console.info(chalk.blue('Tax rule for country ' + country + ' changed to function ' + def))
          return taxRule
        } else {
          console.error(chalk.red('Failed to evaluate tax rule for country ' + country + ' from ' + def + ', result is not a function'))
          return null
        }
      } catch (e) {
        console.error(chalk.red('Failed to evaluate tax rule for country ' + country + ' from ' + def + ', got: ' + e))
        return null
      }
    }

    return null
  }

  Country.prototype = {
    withConfiguration: function (configuration) {
      this.configuration = configuration
      return this
    },
    applyTax: function (sum) {
      const newRule = lookupForOverridenDefinition(this.configuration, this.name)

      if (newRule == null) {
        return this.taxRule([sum])
      }

      try {
        return newRule([sum])
      } catch (e) {
        console.error(chalk.red('Failed to evaluate tax rule for country ' + this.name + ' falling back to original value, got:' + e))
        return this.taxRule([sum])
      }
    }
  }

  const countryDistributionByWeight = _.reduce(europeanCountries, function (distrib, infos, country) {
    let i
    for (i = 0; i < infos[1]; i++) {
      distrib.push(country)
    }
    return distrib
  }, [])
  _.shuffle(countryDistributionByWeight)

  const countryMap = _.reduce(europeanCountries, function (map, infos, country) {
    map[country] = new Country(country, defaultTaxRule(country))
    return map
  }, {})

  return {
    fromEurope: Object.keys(countryMap),

    randomOne: function () {
      return _.sample(countryDistributionByWeight)
    },

    taxRule: function (countryName) {
      const country = countryMap[countryName]
      return country.withConfiguration(this.configuration)
    },

    updateTax: function (country, taxRule) {
      countryMap[country].taxRule = taxRule
    }
  }
})()

module.exports = {
  Sellers,
  Countries
}
