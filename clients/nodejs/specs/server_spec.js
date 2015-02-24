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
  
  it('should respond with hello world', function(done) {
    request('http://' + conf.host + ':' + conf.port+ '/hello', function(error, response, body){
      expect(response.statusCode).toBe(200);
      expect(body).toBe('Hello World\n');
      done();
    });
  });
});
