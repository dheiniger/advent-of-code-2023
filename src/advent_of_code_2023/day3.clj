(ns advent-of-code-2023.day3
  (:require [advent-of-code-2023.util :as util]
            [clojure.string :as str]))

(def input (util/read-lines "input/day3/input.txt"))
(def test-input (util/read-lines "input/day3/test.txt"))

(defn digit? [c]
  (Character/isDigit c))

(defn char-symbol? [c]
  (and
   (not= \. c)
   (not (Character/isDigit c))))

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
    (first (map (fn[[x val]]
                  [x y val])
                (filter #(= x (first %)) (nth grid y))))
    (catch IndexOutOfBoundsException e nil)));;ugly hack

(defn- get-search-coords[x y]
  [[(dec x) y][(inc x) y]
   [(dec x) (dec y)][x (dec y)]
   [(inc x) (dec y)][(dec x) (inc y)]
   [x (inc y)][(inc x) (inc y)]])

(defn- get-search-vals [search-coords grid]
  (remove nil? (map #(get-coord-val % grid) search-coords)))

(defn- group-contains-symbol?
  [num-group y-index full-grid]
  (let [search-coords (partition 2 (flatten
                                    (map #(get-search-coords (first %) y-index)
                                         num-group)))
        search-vals (get-search-vals search-coords full-grid)]
    (some #(char-symbol? (last %)) search-vals)))

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

;;part2
(defn- gear-symbol? [c]
  (= \* c))

(defn- num-group->integer [num-group]
  (->> num-group
       (map second)
       (apply str)
       (Integer/parseInt)))

(defn- get-gear-pos [num-group y-index full-grid]
  (let [search-coords (->> num-group
                           (map #(get-search-coords (first %) y-index))
                           (flatten)
                           (partition 2))
        gear-positions (filter #(gear-symbol? (last %))
                               (get-search-vals search-coords full-grid))]
    (set (map (fn[gear-pos]
                {:xy [(first gear-pos) (second gear-pos)]
                 :num-group [(num-group->integer num-group)]}) gear-positions))))

(defn- find-gear-candidates[input]
  (let [full-grid (make-full-grid input)
        num-grid (map-indexed vector (make-num-grid input))]
    (for [[y-index line] num-grid
          num-group line
          :let [gear-positions (get-gear-pos num-group y-index full-grid)]
          :when (not (empty? gear-positions))]
      gear-positions)))

(defn- find-gears [gear-candidates]
  (let [all-candidates (map #(apply merge %) gear-candidates)
        all-candidates (reduce (fn[acc cand]
                                 (if-let [existing (first (filter #(= (:xy cand) (:xy %)) acc))]
                                   (replace {existing (update-in existing [:num-group] conj (first(:num-group cand)))} acc)
                                   (conj acc cand)))
                               #{}
                               all-candidates)]
    (filter #(= 2 (count (:num-group %))) all-candidates)))

(defn solve-2 [input]
  (let [gears (find-gears (find-gear-candidates input))]
    (->> gears
         (map :num-group)
         (map #(apply * %))
         (reduce +))))

(comment
  (time (solve input))
  (time (solve-2 input)))
