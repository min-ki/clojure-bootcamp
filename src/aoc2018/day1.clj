(ns aoc2018.day1
  (:require [utils.file :refer [read-file-and-to-integer-vector]]))


(defn solve-part1-1 [input]
  (loop [[first & rest-items :as in] input
         out 0]
    (if (nil? in)
      out
      (recur rest-items
             (+ out first)))))

(defn solve-part1-2 [input]
  (reduce + 0 input))


(defn solve-part2-1 [input]
  (loop [[change-value & rest-items] (cycle input)
         frequency #{0}
         current-frequency 0]
    (let [resulting-frequency (+ current-frequency change-value)]
      (if (contains? frequency resulting-frequency)
        resulting-frequency
        (recur rest-items
               (conj frequency resulting-frequency)
               resulting-frequency)))))


(defn solve-part2-2 [input]
  (reduce (fn [acc current]
            (let [resulting-frequency (+ (:result acc) current)]
              (if (contains? (:frequency acc) resulting-frequency)
                (reduced resulting-frequency)
                {:result resulting-frequency :frequency (conj (:frequency acc) resulting-frequency)})))
          {:result 0 :frequency #{0}}
          (cycle input)))


(comment
  (def input (read-file-and-to-integer-vector "resources/aoc2018/day1.txt"))

  (Integer/parseInt "-10")
  (Integer/parseInt "10")

  ;; day1 - part1
  (solve-part1-1 input)
  (solve-part1-2 input)

  (take 10 (cycle input))

  ;; day1 - part2
  (solve-part2-1 [1 -1]);; 0
  (solve-part2-1 [3 3 4 -2 -4]) ;; 10
  (solve-part2-1 [-6 3 8 5 -6])
  (solve-part2-1 [7 7 -2 -7 -4])
  (solve-part2-1 input)

  (solve-part2-2 [1 -1])
  (solve-part2-2 [3 3 4 -2 -4])
  (solve-part2-2 [-6 3 8 5 -6])
  (solve-part2-2 [7 7 -2 -7 -4])
  (solve-part2-2 input))
