(ns extremecarpaccio.core
  (:require [compojure.core :refer [routes POST]]
            [compojure.route :refer [not-found]]
            [ring.util.response :refer [response]]))

(def app
  (routes
    (POST "/ping" [] (response "pong"))
    (not-found "Not Found")))
