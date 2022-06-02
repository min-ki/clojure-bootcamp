(ns aoc2018.day2
  (:require
   [clojure.string :refer [split-lines]]
   [clojure.math.combinatorics :refer [combinations]]))

(def input (->> (slurp "resources/aoc2018/day2.txt")
                split-lines))

(defn str->character-frequencies [s]
  (->> s
       frequencies
       vals))

(defn count-duplicated-numbers [n freqs]
  (->> freqs
       (filter (fn [freqs]
                 (some #(= n %) freqs)))
       count))

(defn diff-one-character-exactly [[box-id1 box-id2]]
  (map (fn [c1 c2] (when (= c1 c2) c1)) box-id1 box-id2))

(comment
  (->> input
       (map str->character-frequencies)
       (count-duplicated-numbers 2))

  (let [duplicated-numbers (map str->character-frequencies input)]
    (* (count-duplicated-numbers 2 duplicated-numbers)
       (count-duplicated-numbers 3 duplicated-numbers)))

  (->> input
       (#(combinations % 2))
       (map diff-one-character-exactly)
       (filter (fn [l] (= 1
                          (count (filter nil? l)))))
       first
       (apply str)))
