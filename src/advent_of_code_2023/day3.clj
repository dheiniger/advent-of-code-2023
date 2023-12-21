(ns advent-of-code-2023.day3
  (:require [advent-of-code-2023.util :as util]
            [clojure.string :as str]))

(def test-input (util/read-lines "input/day3/test.txt"))
(def input (util/read-lines "input/day3/input.txt"))

(defn digit? [c]
  (Character/isDigit c))

(defn char-symbol? [c]
  (and
   (not= \. c)
   (not (Character/isDigit c))))

(defn- get-grid []
  (for [[y line] (map-indexed (fn [idx item] [idx item]) input)
        [x c]    (map-indexed (fn [idx item] [idx item]) line)]
    {:y y :x x :val c}))


(defn- determine-search-coords [{:keys[x y] :as grid-point} grid]
  (let [x-points #{(dec x) x (inc x)}
        y-points #{(dec y) y (inc y)}]
    (for [x_ x-points
          y_ y-points
          :let [val (->> grid (filter #(and (= (:y %1) y_) (= (:x %1 ) x_)));;TODO I think this can be optimized
                         first
                         :val)]
          :when (and (not (nil? val))
                     (not (and (= x x_)
                               (= y y_))))]
      {:x x_ :y y_ :val val})))

(defn- keep-val? [search-points]
  (filter #(char-symbol? (:val %)) search-points))

(defn- nums-with-adjacent-symbols[grid]
  (for [grid-point grid
        :let [check-val (:val grid-point)
              search-coords (determine-search-coords grid-point grid)
              contains-symbol? (some #(char-symbol? (:val %)) search-coords)]
        :when (and (digit? check-val) contains-symbol?)]
    grid-point))

(defn- get-num-groups[grid]
  (->> grid;;TODO -ITERATION
       (partition-by #(digit? (:val %)))
       (map-indexed (fn[idx itm] [(inc idx) itm]))
       (filter #(odd? (first %)))
       (map second)))

(defn- get-digits-to-keep[grid]
  (let [nums-with-adjacent-symbols (into #{} (nums-with-adjacent-symbols grid))]
    (for [num-group (get-num-groups grid)
          :let [num-set (into #{} num-group)]]
      (when-not (empty? (clojure.set/intersection num-set nums-with-adjacent-symbols))
        num-group))))

(defn get-nums-to-keep[grid]
  (let [digits-to-keep (get-digits-to-keep grid)
        ]
    (for [nums digits-to-keep
          :when (not (empty? nums))]
      (->> nums
           (map :val)
           (apply str)
           (Integer/parseInt)))))

(defn solve[grid]
  (reduce + (get-nums-to-keep grid)))
(comment
  (time (solve (get-grid)))
  (solve (take 100 (get-grid)))
  (time (nums-with-adjacent-symbols (get-grid)))
  (time (get-digits-to-keep (get-grid)))
  (get-nums-to-keep)
  (nums-with-adjacent-symbols)
  (map #(determine-search-coords % (get-grid)) (get-grid))
  (first (get-grid))
  (keep even? [1 2 3 4 5 6])

  (last (take 1000 (get-grid))))

