var url = require('url');
var utils = require('../utils');
var UrlAssembler = require('url-assembler');

module.exports = SellerService;

function SellerService (_sellers) {
  this.sellers = _sellers;
}

var service = SellerService.prototype;

service.addCash = function (seller, amount, currentIteration) {
  this.sellers.updateCash(seller.name, amount, currentIteration);
}
service.deductCash = function (seller, amount, currentIteration) {
  this.sellers.updateCash(seller.name, -amount, currentIteration);
}
service.getCashHistory = function (chunk) {
  var cashHistory = this.sellers.cashHistory;
  var cashHistoryReduced = {};
  var lastIteration;

  var seller;
  for(seller in cashHistory) {
    cashHistoryReduced[seller] = [];

    var i = 0;
    for(; i < cashHistory[seller].length; i++) {
      if((i + 1) % chunk === 0) {
        cashHistoryReduced[seller].push(cashHistory[seller][i]);
      }
    }

    if(i % chunk !== 0) {
      cashHistoryReduced[seller].push(cashHistory[seller][i - 1]);
    }

    lastIteration = i;
  }

  return {history: cashHistoryReduced, lastIteration: lastIteration};
}

service.register = function (sellerUrl, name) {
  var parsedUrl = url.parse(sellerUrl);
  var seller = {
    name: name,
    hostname: parsedUrl.hostname,
    port: parsedUrl.port,
    path: parsedUrl.path,
    cash: 0.0,
    online: false,
    url: new UrlAssembler(sellerUrl)
  };
  this.sellers.add(seller);
  console.info('New seller registered: ' + utils.stringify(seller))
}

service.allSellers = function () {
  return this.sellers.all();
}

service.updateCash = function (seller, expectedBill, actualBill, currentIteration) {
  try {
    var totalExpectedBill = utils.fixPrecision(expectedBill.total, 2);
    var totalActualBill = utils.fixPrecision(actualBill.total, 2);

    if(actualBill && totalExpectedBill === totalActualBill) {
      this.addCash(seller, totalExpectedBill, currentIteration);
      this.notify(seller, {'type': 'INFO', 'content': 'Hey, ' + seller.name + ' earned ' + totalExpectedBill});
    }

    else {
      var loss = utils.fixPrecision(totalExpectedBill * .5, 2);
      this.deductCash(seller, loss, currentIteration);
      var message = 'Goddamn, ' + seller.name + ' replied ' + totalActualBill + ' but right answer was ' +  totalExpectedBill + '. ' + loss + ' will be charged.';
      this.notify(seller, {'type': 'ERROR', 'content': message});
    }
  }
  catch (exception) {
    this.notify(seller, {'type': 'ERROR', 'content': exception.message});
  }
}

service.setOffline = function (seller) {
  this.sellers.setOffline(seller.name);
}

service.setOnline = function (seller) {
  this.sellers.setOnline(seller.name);
}

service.notify = function (seller, message) {
  utils.post(seller.hostname, seller.port, seller.path + '/feedback', message);

  if(message.type === 'ERROR') {
    console.error('Notifying ' + seller.name + ': ' + message.content);
  } else {
    console.info('Notifying ' + seller.name + ': ' + message.content);
  }
}
