(ns evolve.core
  (:require [clojure.contrib.string :as contstr])
  (:gen-class))

(defn fitness [target]
  (fn [source]  
    (apply + 
      (map 
        (fn [x y] (* (- x y) (- x y))) 
        (contstr/codepoints source) 
        (contstr/codepoints target))) ))

(defn mutate [source]
  (let [i (rand-int (count source))] 
    (str 
      (subs source 0 i) (char (inc (int (nth source i)))) (subs source (inc i)))))

(defn evolve
  [source fitness]
  (first 
    (filter #(< (fitness %) (fitness source)) 
      (iterate mutate source))))

(defn start
  [generations source target]
  (nth (iterate #(evolve % (fitness target)) source) generations))

(defn -main [& args]
  (println (start 500 "dlk33ndoemgl" "Hello World!")))
