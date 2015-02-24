var conf = require('./conf'),
    server = require('./lib/server'),
    applyLogic = require('./lib/logic');

server.start(conf, applyLogic);
