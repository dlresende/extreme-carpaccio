#lang racket

(provide handle-request)

(define plain-text
  '((header . "Content-Type") "text/plain; charset=utf-8"))

;; Returns three values: status, headers and body. Kept free of any web server
;; type so the routing rules can be tested without starting a server.
(define (handle-request method path)
  (cond
    [(and (eq? method 'POST) (string=? path "/ping"))
     (values 200 plain-text "pong")]
    [else
     (values 404 plain-text "Not Found")]))
