(ns advent-of-code-2023.day2
  (:require [advent-of-code-2023.util :as util]
            [clojure.string :as str]))

(def test-input (util/read-file "input/day2/test.txt"))
(def input (util/read-file "input/day2/input.txt"))
(def bag-contents {:red 12 :green 13 :blue 14})

(defn- get-id[str]
  (Integer/parseInt (re-find #"\d+" str)))

(defn- get-num [cube-amt]
  (or (when cube-amt
        (-> cube-amt
            (str/split #" ")
            first
            (Integer/parseInt)))
      0))

(defn- get-cubes[str]
  (let [red-pattern   #"\d+ red"
        green-pattern #"\d+ green"
        blue-pattern  #"\d+ blue"]
    {:red (get-num (re-find red-pattern str))
     :blue (get-num (re-find blue-pattern str))
     :green (get-num (re-find green-pattern str))}))

(defn- get-sets[str]
  (let [str (str/trim (subs str (inc (str/index-of str ":"))))]
    (str/split str #";")))

(defn- impossible-set? [set]
  (let [cubes (get-cubes set)]
    (or (> (:red cubes) (:red bag-contents))
        (> (:blue cubes) (:blue bag-contents))
        (> (:green cubes) (:green bag-contents)))))

(defn- possible-game? [game]
  (let [sets (get-sets game)]
    (empty? (filter #(impossible-set? %) sets))))

(defn part-1 []
  (let [games (str/split-lines input)]
    (->> games
         (filter possible-game?)
         (map #(get-id %))
         (reduce +))))

(part-1)

(defn- smallest-set [game]
  (let [sets (get-sets game)
        cubes (map get-cubes sets)]
    (letfn [(max-color [color] (apply max (map color cubes)))]
      {:max-red (max-color :red)
       :max-blue (max-color :blue)
       :max-green (max-color :green)})))

(defn- power [set-map]
  (let [{:keys[max-red max-blue max-green]} set-map]
    (* max-red max-blue max-green)))

(defn part-2 []
  (let [games (str/split-lines input)]
    (->> games
         (map #(smallest-set %))
         (map #(power %))
         (reduce +))))

(part-2)

