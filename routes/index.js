var express = require('express');
var router = express.Router();

/* GET home page. */
router.get('/', function(req, res, next) {
  res.render('index', { title: 'Express' });
});

router.get('/register', function(req, res, next) {
    var html = '<form action="/register" method="post">' +
        'Enter your URL:' +
        '<input type="text" name="userUrl" placeholder="http://192.168.1.11:3000" />' +
        '<br>' +
        '<button type="submit">Submit</button>' +
        '</form>';

    res.send(html);
});

router.post('/register', function(req, res, next) {
    var userUrl = req.body.userUrl;
    var html = 'Hello: ' + userUrl + '.<br>';
    res.send(html);
});

module.exports = router;
