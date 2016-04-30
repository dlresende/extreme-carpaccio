var _ = require('lodash');

function PayThePriceReduction () {
  this.name = 'PAY THE PRICE';
  this.apply = function (amount) {
    return amount;
  }
}

function HalfPriceReduction () {
  this.name = 'HALF PRICE';
  this.apply = function (amount) {
    return amount / 2;
  }
}

function ReductionStep (sum, reduction) {
  this.sum = sum;
  this.reduction = reduction;
}

function StandardReduction(sum, reduction) {
  this.name = 'STANDARD';
  var reductions = [
    new ReductionStep(50000, 0.15),
    new ReductionStep(10000, 0.10),
    new ReductionStep(7000, 0.07),
    new ReductionStep(5000, 0.05),
    new ReductionStep(1000, 0.03)
  ]

  this.apply = function (amount) {
    return amount * (1 - this.reductionFor(amount));
  }
  this.reductionFor = function (total) {
    var reduction = _.result(_.find(reductions, function(reduc) { return reduc.sum <= total; }), 'reduction');
    if(reduction === undefined) {
      return 0;
    }
    return reduction;
  }
}

exports.STANDARD = new StandardReduction();
exports.PAY_THE_PRICE = new PayThePriceReduction();
exports.HALF_PRICE = new HalfPriceReduction();
