(ns extremecarpaccio.core
  (:require [compojure.core :refer [defroutes routes GET POST]]
            [compojure.route :refer [not-found]]
            [compojure.handler :refer [site]]
            [ring.adapter.jetty :refer [run-jetty]]
            [clojure.pprint :refer [pprint]]
            [ring.middleware.defaults :refer :all]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.middleware.format :refer [wrap-restful-format]]
            [aprint.core :refer :all]
            [ring.util.response :refer [response]]
            ))

(def country->tax
  {"BG" 10
   "CZ" 11
   "DK" 12
   "DE" 13
   "EE" 14
   "IE" 15
   "EL" 16
   "ES" 17
   "FR" 18
   "HR" 19
   "IT" 20
   "CY" 10
   "LV" 11
   "LT" 12
   "LU" 13
   "HU" 14
   "MT" 15
   "NL" 16
   "AT" 17
   "PL" 18
   "PT" 19
   "RO" 20
   "SI" 10
   "SK" 11
   "FI" 12
   "SE" 13
   "UK" 14})

(defn calculate-tax-factor [country]
  (/ (+ 100 (or (country->tax country) 0)) 100)
  )


(defn calculate-discount [amount]
  (/ (- 100 (cond
              (>= amount 50000) 15
              (>= amount 10000) 10
              (>= amount 7000) 7
              (>= amount 5000) 5
              (>= amount 1000) 3
              :default 0))
     100))

(defn calculate-gross-total [prices quantities]
  (reduce (fn [acc [k v]] (+ acc (* k v))) 0
          (zipmap prices quantities)))


(def name->reduction {"STANDARD" calculate-discount
                      "HALF PRICE" (fn [_] 0.5)
                      "PAY THE PRICE" (fn [_] 1)})


(defn handle-order [req]
  (let [body            (:body-params req)
        gross-total     (calculate-gross-total (:prices body) (:quantities body))
        tax-factor      (calculate-tax-factor (:country body))
        discount-factor ((name->reduction (:reduction body)) (* tax-factor gross-total))
        total           {:total (* discount-factor tax-factor gross-total)}]
    (response total)))


(defn print-request
  "Middleware which pretty prints all requests"
  [handler]
  (fn [request]
    (aprint request)
    (handler request)))

(defn handle-feedback [req]
  (let [body (:body-params req)
        ]
    (println (:type body) ":" (:content body))
    (response nil))
  )

(def my-app
  (-> (routes
        (POST "/feedback" [] handle-feedback)
        (print-request (POST "/order" [] handle-order))
        (GET "/ping" [] "pong")
        (GET "/" [] "I'm alive!")
        (not-found "No page"))
      (wrap-restful-format :formats [:json-kw])))

(defonce server (run-jetty #'my-app {:port 4000 :join? false}))

