var _ = require('lodash');

var Reduction = (function(){
    var PayThePriceReduction = function() {
        this.name = 'PAY THE PRICE';
    };
    PayThePriceReduction.prototype = {
        apply: function(amount) {
            return amount;
        }
    };

    var HalfPriceReduction = function() {
        this.name = 'HALF PRICE';
    };
    HalfPriceReduction.prototype = {
        apply: function(amount) {
            return amount * (1 - .5);
        }
    };

    var StandardReduction = function() {
        this.name = 'STANDARD';
    };
    StandardReduction.prototype = (function (){
        var Reduction = function(sum, reduction) {
            this.sum = sum;
            this.reduction = reduction;
        };

        var reductions = [
            new Reduction(50000, 0.15),
            new Reduction(10000, 0.10),
            new Reduction(7000, 0.07),
            new Reduction(5000, 0.05),
            new Reduction(1000, 0.03)
        ];

        return {
            apply: function(amount) {
                return amount * (1 - this.reductionFor(amount));
            },

            reductionFor: function(total) {
                var reduction = _.result(_.find(reductions, function(reduc) { return reduc.sum <= total; }), 'reduction');

                if(reduction === undefined) {
                    return 0;
                }

                return reduction;
            }
        };
    })();

    return {
        STANDARD : new StandardReduction(),
        PAY_THE_PRICE : new PayThePriceReduction(),
        HALF_PRICE: new HalfPriceReduction()
    }
})();

module.exports = Reduction;
