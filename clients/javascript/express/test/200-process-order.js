var expect = require('chai').expect;
var process = require('../lib/process');

describe('process.order(payload, callback)', function () {
  it('calls back with an empty object', function (done) {
    process.order({}, function (err, result) {
      if (err) return done(err);
      expect(result).to.deep.equal({});
      done();
    })
  })
})
