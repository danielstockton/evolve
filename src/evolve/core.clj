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

(defn popfitness
  "maps a fitness to each member of the population"
  [population fitness]
  (into [] (map #(vector % (fitness %)) population)))

(defn selection
  "selects two parents with a higher probability of selecting fit parents"
  [popfitness]
  (first
    (last
      (remove
        #(< (rand) (second %))
        (let
          [fits (map #(/ 1 (+ 0.0000001 (second %))) popfitness)
           tot-fit (reduce + fits)
           cum-fits (reductions + 0 fits)
           roulette (for [k (range (count popfitness))] [(first (nth popfitness k)) (float (/ (nth cum-fits k) tot-fit))])]
          roulette)))))

(defn mutate [source]
  (let [i (rand-int (count source))] 
    (str 
      (subs source 0 i) (char (+ (rand-nth [-1 1]) (first (contstr/codepoints (str (nth source i)))))) (subs source (inc i)))))

(defn breed 
  "breeds two parents together to create one offspring"
  [parent1 parent2]
  (let [l1 (count parent1) l2 (count parent2)] 
    (mutate 
      (apply str 
             (concat 
               (subs parent1 0 (/ l1 2)) 
               (subs parent2 (/ l2 2) l2))))))

(defn evolve
  [population target]
  (repeatedly (count population) #(breed (selection (popfitness population (fitness-fn target))) (selection (popfitness population (fitness-fn target))))))


(defn start
  [generations popsize target] 
  (let [populate (population popsize (count target))] 
    (println (nth (iterate #(evolve % target) populate) generations))))

(defn -main [generations popsize target]
  (start (new Integer generations) (new Integer popsize) target))
