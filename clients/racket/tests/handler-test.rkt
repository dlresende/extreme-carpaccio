#lang racket

(require rackunit
         "../handler.rkt")

(module+ test
  (define-values (status headers body) (handle-request 'POST "/ping"))
  (check-equal? status 200)
  (check-equal? body "pong"))
