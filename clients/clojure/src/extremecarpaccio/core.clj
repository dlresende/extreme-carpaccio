(ns extremecarpaccio.core
  (:require [compojure.core :refer [defroutes GET]]
            [compojure.route :refer [not-found]]
            [compojure.handler :refer [site]]
            [ring.adapter.jetty :refer [run-jetty]]
            [clojure.pprint :refer [pprint]]))

(defn pong [req]
  (pprint req)
  {:status 200
   :headers {"Content-Type" "text/html"}
   :body "pong"})

(defroutes routes
           (GET "/" [] "I'm alive!")
           (GET "/ping" [] pong)
           (not-found "No page"))

(defonce server (run-jetty #'my-app {:port 8080 :join? false}))

