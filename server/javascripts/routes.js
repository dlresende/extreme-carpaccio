const express = require('express')
const _ = require('lodash')

module.exports = function (sellerService, dispatcher) {
  const router = express.Router()
  const OK = 200
  const BAD_REQUEST = 400
  const UNAUTHORIZED = 401

  router.get('/sellers', function (request, response) {
    // seller view is returned, to prevent any confidential information leaks
    const sellerViews = _.map(sellerService.allSellers(), function (seller) {
      return {
        cash: seller.cash,
        name: seller.name,
        online: seller.online
      }
    })
    response.status(OK).send(sellerViews)
  })

  router.get('/sellers/history', function (request, response) {
    const chunk = request.query.chunk || 10
    response.status(OK).send(sellerService.getCashHistory(chunk))
  })

  router.post('/seller', function (request, response) {
    const sellerName = request.body.name
    const sellerUrl = request.body.url
    const sellerPwd = request.body.password

    if (_.isEmpty(sellerName) || _.isEmpty(sellerUrl) || _.isEmpty(sellerPwd)) {
      response.status(BAD_REQUEST).send({ message: 'missing name, password or url' })
    } else if (!sellerService.isUrlValid(sellerUrl)) {
      response.status(BAD_REQUEST).send({ message: 'invalid url' })
    } else if (sellerService.isAuthorized(sellerName, sellerPwd)) {
      sellerService.register(sellerUrl, sellerName, sellerPwd)
      response.status(OK).end()
    } else {
      response.status(UNAUTHORIZED).send({ message: 'invalid name or password' })
    }
  })

  return router
}
