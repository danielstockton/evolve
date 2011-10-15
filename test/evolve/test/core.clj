(ns evolve.test.core
  (:use [evolve.core])
  (:use [clojure.test]))

(deftest fitness
  (is ((fitness-fn "jimmy") "james") 164))
