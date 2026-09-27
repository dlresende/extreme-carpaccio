(ns extremecarpaccio.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [extremecarpaccio.core :as core]))

(deftest post-ping-responds-with-pong
  (testing "POST /ping"
    (let [response (core/app {:request-method :post
                              :uri "/ping"})]
      (is (= 200 (:status response)))
      (is (= "pong" (:body response))))))
