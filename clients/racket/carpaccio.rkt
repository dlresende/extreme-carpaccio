#lang racket

(provide carpaccio/total)

(define (carpaccio/total details country reduction)
  0.0)


(module+ test 
  
  (require rackunit rackunit/text-ui)
  
  (check-equal? 0.0 (carpaccio/total '() "" ""))
  
  "all test run")