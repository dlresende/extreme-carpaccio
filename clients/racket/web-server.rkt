#lang racket

(require web-server/servlet
         web-server/servlet-env
         json)

;; Your main aglorithm is here.
;; Code it using TDD cycles ;-)
(require "carpaccio.rkt")  

(provide main)

(define (json-from-post-data req)
  (let ([data (request-post-data/raw req)])
    (if data (bytes->jsexpr data)
        (json-null))))
      
(define (response/jsonp o)
  (response/output
   (lambda (op)
     (write-json o op))
   #:mime-type #"application/json"))
  
(define (start req)
  (carpaccio-dispatch req))

;; Dispatches accepted paths
(define-values (carpaccio-dispatch req-url)
  (dispatch-rules
   (("ping") handle-get-pong)
   (("order") #:method "post" handle-post-order)
   (("feedback") #:method "post" handle-post-feedback)))

;; GET /pong
(define (handle-get-pong req)
  (response/xexpr 
   '(html (body (p "Pong")))))

;; POST /feedback
(define (handle-post-feedback req)
  (define message (json-from-post-data req))
  (printf "\n**** Feedback=~s ~s\n\n"
          (hash-ref message 'type) 
          (hash-ref message 'content))
  (flush-output)
  (response/xexpr '(html (body "thanx for feedback"))))

;; POST /order
(define (handle-post-order req)
  (define command (json-from-post-data req))
  
  (define prices (hash-ref command 'prices))
  (define quantities (hash-ref command 'quantities))
  (define details 
    (for/list ([q quantities]
               [p prices])
      (cons q p)))
  (define reduction (hash-ref command 'reduction))
  (define country (hash-ref command 'country))
  (define resp (carpaccio/total details country reduction))

  (printf " Received=~s\n" details)
  (printf "Reduction=~s\n" reduction)
  (printf "  Country=~s\n" country)
  (printf "  Reply-->~s\n" resp)
  (flush-output)
  (response/jsonp (hasheq 'total resp)))

;; Run it from command line: 
;; $ racket -tm web-server.rkt
(define (main . xs)
  (serve/servlet start
                 #:port 8080            ; web server port
                 #:listen-ip #f         ; Listen whatever the ip or hostname 
                 #:servlet-path ""
                 #:servlet-regexp #rx"" ; regex used to decide if this url should be handled
                 #:command-line? #t))
