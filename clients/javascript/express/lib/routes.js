exports.order = function order(req, res, next) {
  // TODO implement from here
  res.json({});
}

exports.feedback = function feedback(req, res, next) {
  console.info("FEEDBACK:", req.body.type, req.body.content);
  next();
}
