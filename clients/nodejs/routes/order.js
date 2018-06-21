var express = require('express');
var router = express.Router();

/* GET order listing. */
router.post('/', function(req, res, next) {
  res.status(404);
  console.log(req.body);
  res.json({});
});

module.exports = router;
