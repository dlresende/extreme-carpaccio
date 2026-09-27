(ns extremecarpaccio.server
  (:gen-class)
  (:require [ring.adapter.jetty :as jetty]
            [ring.util.response :refer [response]]
            [extremecarpaccio.core :as core]))

(defn -main [& _]
  (let [port (Integer/parseInt (or (System/getenv "PORT") "3000"))]
    (println (str "Listening on http://0.0.0.0:" port))
    (jetty/run-jetty #'core/app {:port port :join? true})))
