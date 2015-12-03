var conf = require('../conf'),
    request = require('request'),
    server = require('../lib/server'),
    serverInstance;

describe('Default server', function () {
  beforeEach(function() {
    serverInstance = server.start(conf, function() {
      return {total:500};
    });
  });
  afterEach(function() {
    serverInstance.close();
  });
  
  it('should handle feedback', function(done) {
    var feedback = request('http://' + conf.host + ':' + conf.port + '/feedback', function (error, response) {
      expect(response.statusCode).toBe(204);
      done();
    });
    feedback.write('{type: "INFO", content: "this is my precious feedback"}');
  });
  
  it('should handle order', function(done) {
    var order = request('http://' + conf.host + ':' + conf.port + '/order', function (error, response, body) {
      expect(response.statusCode).toBe(200);
      expect(body).toBe('{total:0}');
      done();
    });
    order.write('{prices: [], quantities: [], country: "DE", reduction: "STANDARD"}');
  });
});
