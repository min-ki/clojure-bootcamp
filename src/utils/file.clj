(ns utils.file
  (:require [clojure.string :refer [split-lines]]))

(defn read-file-and-to-integer-vector [file]
  (map #(Integer/parseInt %) (split-lines (slurp file))))

(defn read-file-and-to-string-vector [file]
  (split-lines (slurp file)))

(comment
  (read-file-and-to-integer-vector "resources/aoc2018/day1.txt")
  (take 10 (cycle (read-file-and-to-integer-vector "resources/aoc2018/day1.txt")))

  (read-file-and-to-string-vector "resources/aoc2018/day2.txt")
  (take 10 (read-file-and-to-string-vector "resources/aoc2018/day2.txt")))
