var express = require('express');
var router = express.Router();

/* GET feeback listing. */
router.post('/', function(req, res, next) {
  console.log(req.body)
  res.json({});
});

module.exports = router;
