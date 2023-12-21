(ns advent-of-code-2023.util
  (:require [clojure.string :as str]))

(defn read-file [file-path]
  (slurp file-path))

(defn read-lines [file-path]
  (str/split-lines (read-file file-path)))

