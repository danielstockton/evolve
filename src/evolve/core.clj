(ns evolve.core
  (:require [clojure.contrib.string :as contstr])
  (:gen-class))

(defn fitness-fn [target]
  (fn [source]  
    (apply + 
      (map 
        (fn [x y] (* (- x y) (- x y))) 
        (contstr/codepoints source) 
        (contstr/codepoints target)))))

(defn mutate [source]
  (let [i (rand-int (count source))] 
    (str 
      (subs source 0 i) (char (+ (rand-nth [-1 1]) (first (contstr/codepoints (str (nth source i)))))) (subs source (inc i)))))

(defn evolve
  [source fitness]
  (first 
    (filter #(<= (fitness %) (fitness source)) 
      (iterate mutate source))))

(defn start
  [generations source target]
  (nth (iterate #(evolve % (fitness-fn target)) source) generations))

(defn -main [source target]
  (println (start 50 source target)))
