(ns evolve.core
  (:require [clojure.contrib.string :as contstr])
  (:gen-class))

(defn randchar
  "returns a random character from ascii codes 33-126"
  []
  (char (nth (range 33 126) (rand (count (range 33 126))))))

(defn randstring 
  "returns a randomised string of length l"
  [l]
  (apply str (take l (repeatedly randchar))))

(defn population
  "generates population of n random strings of length l" 
  [n l]
  (repeatedly n #(randstring l)))

(defn fitness-fn
  "returns a function that accepts a string and returns its fitness compared to the supplied target" 
  [target]
  (fn [source]  
    (apply + 
      (map 
        (fn [x y] (* (- x y) (- x y))) 
        (contstr/codepoints source) 
        (contstr/codepoints target)))))

(defn pop-fitness
  "maps a fitness to each member of the population"
  [population fitness]
  (map 
    #(fitness %)
    population))

(defn mutate 
  [source]
  (let [i (rand-int (count source))] 
    (str 
      (subs source 0 i) (char (+ (rand-nth [-1 1]) (first (contstr/codepoints (str (nth source i)))))) (subs source (inc i)))))

(defn evolve
  [source fitness]
  (if 
    (= (fitness source) 0) 
    (str source) 
    (first 
      (filter #(<= (fitness %) (fitness source)) 
        (repeatedly #(mutate source))))))

(defn start
  [generations source target]
  (nth (map-indexed (fn [idx itm] (doto (str idx " " ((fitness-fn target) itm) " " itm) println)) (iterate #(evolve % (fitness-fn target)) source)) generations))

(defn -main [generations source target]
  (start (new Integer generations) source target))
