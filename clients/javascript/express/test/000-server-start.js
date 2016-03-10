var supertest = require('supertest');
var Server = require('../server');

describe('the server', function () {
  var previousPort, server;
  beforeEach(function () {
    previousPort = process.env.PORT;
    process.env.PORT = 21345;
  })
  afterEach(function () {
    process.env.PORT = previousPort;
  });

  beforeEach(function (done) {
    server = new Server();
    server.start(done);
  })

  afterEach(function (done) {
    server.stop(done);
  })

  it('starts on the port in PORT', function (done) {
    supertest('http://localhost:21345')
      .get('/status')
      .expect(200)
      .expect({up: true})
      .end(done)
  });
})
