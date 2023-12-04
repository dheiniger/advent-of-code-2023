(ns advent-of-code-2023.day1
  (:require [advent-of-code-2023.util :as util]
            [clojure.string :as str]))

(def input (util/read-file "input/day1/input.txt"))
(def p-string "(?=([0-9]|one|two|three|four|five|six|seven|eight|nine))")
(def numbers {"one" "1"
              "two" "2"
              "three" "3"
              "four" "4" 
              "five" "5" 
              "six" "6"
              "seven" "7"
              "eight" "8"
              "nine" "9"})

(defn solve [lines]
  (->> (map #(str/replace % #"[A-Za-z]" "") lines)
       (map #(str (first %) (last %)))
       (map #(Integer/parseInt %))
       (reduce +)))

(solve (str/split-lines input))

(defn- words->nums [str]
  (let [coll (map second (re-seq (re-pattern p-string) str))]
    (map #(str/replace %1 %1 (or (get numbers %1) %1)) coll)))

(defn part-2 [lines]
  (->> (map #(words->nums %) lines)
       (map (juxt first last))
       (map #(apply str %))
       (solve)))

(part-2 (str/split-lines input))


