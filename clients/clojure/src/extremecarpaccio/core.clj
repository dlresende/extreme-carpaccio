(ns extremecarpaccio.core
  (:require [compojure.core :refer [routes GET POST]]
            [compojure.route :refer [not-found]]
            [ring.adapter.jetty :refer [run-jetty]]
            [ring.middleware.format :refer [wrap-restful-format]]
            [ring.util.response :refer [response]]))

(defn handle-feedback [req]
  (let [body (:body-params req)]
    (println (:type body) ":" (:content body))
    (response nil)))

(defn cannot-handle [req]
  (println "Cannot handle " req)
  (response nil))

(def my-app
  (-> (routes
        (POST "/feedback" [] handle-feedback)
        (GET "/ping" [] "pong")
        (GET "/" [] "I'm alive!")
        (not-found cannot-handle))
      (wrap-restful-format :formats [:json-kw])))

(defonce server (run-jetty #'my-app {:port 4000 :join? false}))

