#lang racket

(require web-server/servlet
         web-server/servlet-env
         json)
 
;; ===================================================================

(define (total details country reduction)
  1.0)


;; ===================================================================

(define (json-from-post-data req)
  (let ([data (request-post-data/raw req)])
    (if data (bytes->jsexpr data)
        (json-null))))
      
(define (response/jsonp o)
  (response/output
   (lambda (op)
     (write-json o op))
   #:mime-type #"application/json"))
  
(define (my-app req)
  (define command (json-from-post-data req))
  
  (define prices (hash-ref command 'prices))
  (define quantities (hash-ref command 'quantities))
  (define details 
    (for/list ([q quantities]
               [p prices])
      (list q p)))
  (define reduction (hash-ref command 'reduction))
  (define country (hash-ref command 'country))
  (define resp (total details country reduction))

  (printf " Received=~s\n" details)
  (printf "Reduction=~s\n" reduction)
  (printf "  Country=~s\n" country)
  (printf "  Reply-->~s\n" resp)
  (response/jsonp (hasheq 'total resp)))
 
(serve/servlet my-app
               #:port 8080
               #:servlet-path "/order"
               #:command-line? #t)

