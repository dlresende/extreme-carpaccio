(ns extremecarpaccio.core-test
  (:require [midje.sweet :refer :all]
            [ring.mock.request :as mock]
            [extremecarpaccio.core :refer :all]))

(facts "about routes"
  (fact "ping route should respond pong"
    (let [response (my-app (mock/request :get "/ping"))]
      (:status response) => 200
      (:body response) => "pong"))

  (fact "not-found route should respond 404"
    (let [response (my-app (mock/request :get "/invalid"))]
      (:status response) => 404)))