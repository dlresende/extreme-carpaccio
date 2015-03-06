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


(deftest gross-total
  (let [prices     [63.22 67.27 80.32 78.43 73.87 2.7 32.29
                    16.15]
        quantities [10 1 2 7 10 9 5 5]]
    (is (= 2414 (Math/round (calculate-gross-total prices quantities))))))


(deftest tax-factor
  (are [country tax-factor] (= tax-factor (calculate-tax-factor country))
                            "IT" 120/100
                            "SI" 110/100
                            "UK" 114/100))

(deftest discount
  (are [amount discount] (= (double discount) (double (calculate-discount amount)))
                         51000 0.85
                         49000 0.90))


