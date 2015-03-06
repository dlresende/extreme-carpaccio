(ns user
  (:require [ring.adapter.jetty :refer [run-jetty]]
            [extremecarpaccio.core :refer :all]))

(defonce server (run-jetty #'my-app {:port 4000 :join? false}))
