(ns extremecarpaccio.core-test
  (:require [clojure.test :refer :all]
            [ring.mock.request :as mock]
            [extremecarpaccio.core :refer :all]))

(deftest test-routes
  (testing "main route"
    (let [response (my-app (mock/request :get "/"))]
      (is (= (:status response) 200))
      (is (= (:body response) "I'm alive!"))))

  (testing "not-found route"
    (let [response (my-app (mock/request :get "/invalid"))]
      (is (= (:status response) 404)))))
