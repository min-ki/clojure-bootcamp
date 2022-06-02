(ns aoc2020.day1
  (:require
   [clojure.string :refer [split-lines]]
   [clojure.math.combinatorics :refer [combinations]]))

;; Day 1 : Report Repair
;; 여행을 가기위한 star를 모으기 위해 퍼즐을 풀어야한다.
;; 50개의 별을 모아야한다.

;; Part1
;; 리스트가 주어졌을때 더해서 2020이 되는 요소를 찾는다.
;; 그리고 그것들을 곱한 값이 정답이 된다.

(def input (->> (slurp "resources/aoc2020/day1.txt")
                split-lines))

(defn parse [num-of-entries input]
  (->> input
       (map #(Integer/parseInt %))
       (#(combinations % num-of-entries))))

(defn solve-part1 [entries]
  (->> entries
       (filter (fn [[entry1 entry2]] (= (+ entry1 entry2) 2020)))
       first
       (reduce *)))

(comment
  (->> input
       (parse 2)
       solve-part1))


;; Part2
;; 동일한 조건을 만족시키는 비용 레포트에서 세개의 숫자를 찾는다면 stars를 얻을 수 있다.
;; 2020을 만들기위해 더했을때 979, 366, 675가 되는 조합을 찾는다.

(defn solve-part2 [entries]
  (->> entries
       (filter (fn [[entry1 entry2 entry3]] (= (+ entry1 entry2 entry3) 2020)))
       first
       (reduce *)))

(comment

  (->> input
       (parse 3)
       solve-part2))