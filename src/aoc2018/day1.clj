(ns aoc2018.day1
  (:require [clojure.string :refer [split-lines]]))

(def input (->> (slurp "resources/aoc2018/day1.txt")
                split-lines
                (map #(Integer/parseInt %))))

(defn solve-part1 [input]
  (reduce + 0 input))

(defn find-first-duplicated [seen current]
  (if (contains? seen current)
    (reduced current)
    (conj seen current)))

(defn solve-part2 [input]
  (->> input
       cycle
       (reductions + 0)
       (reduce find-first-duplicated #{})))

(comment
  (->> input
       solve-part1)

  (->> input
       cycle
       solve-part2))

