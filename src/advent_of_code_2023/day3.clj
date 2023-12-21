(ns advent-of-code-2023.day3
  (:require [advent-of-code-2023.util :as util]
            [clojure.string :as str]))

(def test-input (util/read-lines "input/day3/test.txt"))

(defn- get-indexed-numbers[line]
  (let [indexed (map-indexed vector line)]
    (->> indexed
         (partition-by #(digit? (second %)))
         (remove #(not (digit? (first (nfirst %))))))))

(defn- get-indexed-vals[line]
  (map-indexed vector line))

(defn- make-full-grid [input]
  (map get-indexed-vals input))

(defn- make-num-grid [input]
  (map get-indexed-numbers input))

(defn- get-coord-val [[x y] grid]
  (try
    (first (filter #(= x (first %)) (nth grid y)))
    (catch IndexOutOfBoundsException e nil)))

(defn- get-search-coords[x y]
  [[(dec x) y]
   [(inc x) y]
   [(dec x) (dec y)]
   [x (dec y)]
   [(inc x) (dec y)]
   [(dec x) (inc y)]
   [x (inc y)]
   [(inc x) (inc y)]])

(defn- get-search-vals [search-coords grid]
  (remove nil? (map #(get-coord-val % grid) search-coords)))

(defn- group-contains-symbol?
  [num-group y-index full-grid]
  (let [search-coords (partition 2 (flatten
                                    (map #(get-search-coords (first %) y-index)
                                         num-group)))
        search-vals (get-search-vals search-coords full-grid)]
    (some #(char-symbol? (second %)) search-vals)))
    
(defn- get-digits-to-keep [input]
  (let [full-grid (make-full-grid input)
        num-grid (map-indexed vector (make-num-grid input))]
    (for [[y-index line] num-grid
          num-group line
          :let [group-has-symbol? (group-contains-symbol? num-group y-index full-grid)]
          :when group-has-symbol?]
      num-group)))

(defn solve [input]
  (let [digit-groups (get-digits-to-keep input)]
    (->> digit-groups
         (map (fn[group]
                (->> group
                     (map second)
                     (apply str)
                     (Integer/parseInt))))
         (reduce +))))

(time (solve input))

