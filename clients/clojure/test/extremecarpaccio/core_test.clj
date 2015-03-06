(ns extremecarpaccio.core-test
  (:require [clojure.test :refer :all]
            [ring.mock.request :as mock]
            [extremecarpaccio.core :refer :all]))

(deftest test-routes
  (testing "ping route"
    (let [response (my-app (mock/request :get "/ping"))]
      (is (= (:status response) 200))
      (is (= (:body response) "pong"))))

  (testing "not-found route"
    (let [response (my-app (mock/request :get "/invalid"))]
      (is (= (:status response) 404)))))