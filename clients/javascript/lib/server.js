var http = require('http'),
    url = require('url'),
    querystring = require('querystring');

/**
 * Write the content as raw json response.
 *
 * @param {Object} res - http response
 * @param {Object} content - content to send
 * @api private
 */
var writeJson = function (res, content) {
  res.writeHead(200, {'Content-Type': 'application/json'});
  res.end(JSON.stringify(content, null, "  "), "utf-8");
}

/**
 * Load request body, apply logic on it and then send the computed response back.
 *
 * @param {Object} req - http request
 * @param {Object} res - http response
 * @param {Function} applyLogic - function that apply logic
 * @api private
 */
var loadBodyAndProcess = function (req, res, applyLogic) {
  var body = ''; // buffer to store data: assume body will be raw text
  req.on('data', function(chunk) {
    body += chunk;
  });
  req.on('end', function() {
      var reqBody = JSON.parse(body); // assume input is json :)
      console.log('Incoming request data %j', reqBody);

      var resBody = applyLogic(reqBody);
      console.log('Computed response %j', resBody);

      // send the response back
      writeJson(res, resBody);
  });
};


/**
 * Starts the server using the specified conf, all logic is then forwarded to
 * the given callback.
 *
 * @param {Object} conf - configuration
 * @param {Function} applyLogic - function that apply logic
 * @api public
 */
 var start = function(conf, applyLogic) {
   var server = http.createServer(function (req, res) {
     var method = req.method,
         parsedURL = url.parse(req.url),
         params = querystring.parse(parsedURL.query);

     console.log('Incoming request [%s][%s] with %j', method, parsedURL.pathname, params);

     if(method==='POST' && parsedURL.pathname === '/order') {
       loadBodyAndProcess(req, res, applyLogic);
       res.end();
       return;
     }
     res.statusCode = 204;
     res.end();
   });

   server.listen(conf.port, conf.host, function() {
     var address = server.address();
     console.log("Client server started on %j", address);
   });

   return server;
};
module.exports.start = start;
