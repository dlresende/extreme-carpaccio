#lang racket

(require racket/cmdline
         web-server/http-response
         web-server/servlet-env
         "handler.rkt")

(define (dispatch req)
  (define-values (status headers body)
    (handle-request (request-method req) (request-path req)))
  (response* status headers body))

(module+ main
  (define port
    (command-line
     #:program "extreme-carpaccio"
     #:once-each
     [("-p" "--port") port ("Port to listen on" [n (and (string->number n) (exact->inexact (string->number n)))])]))

  (define stop-server
    (serve/servlet dispatch
                   #:port port
                   #:listen-ip #f
                   #:servlet-path ""
                   #:servlet-regexp #rx""))

  (printf "Listening on http://0.0.0.0:~a\n" port)
  (with-handlers ([exn:fail? void])
    (stop-server))
  (void))
