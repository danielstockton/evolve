(ns evolve.test.core
  (:use [evolve.core])
  (:use [clojure.test]))

(deftest fitness-test
  (is 
    (= ((fitness-fn "jimmy") "james") 164)
    "Fitness is calculated incorrectly"))

(deftest failure-test
  (is
    (= 2 3)
    "Definite Failure"))
