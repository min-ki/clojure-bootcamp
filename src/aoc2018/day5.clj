(ns aoc2018.day5
  (:require [clojure.string :refer [trim]]))

(def input (map (comp keyword str) (slurp "resources/aoc2018/day5.txt")))

(def ascii-lowercases (map char (range 97 123)))

(def opposite-unit-pairs
  {:a :A
   :A :a
   :b :B
   :B :b
   :c :C
   :C :c
   :d :D
   :D :d
   :e :E
   :E :e
   :f :F
   :F :f
   :g :G
   :G :g
   :h :H
   :H :h
   :i :I
   :I :i
   :j :J
   :J :j
   :k :K
   :K :k
   :l :L
   :L :l
   :m :M
   :M :m
   :n :N
   :N :n
   :o :O
   :O :o
   :p :P
   :P :p
   :q :Q
   :Q :q
   :r :R
   :R :r
   :s :S
   :S :s
   :t :T
   :T :t
   :u :U
   :U :u
   :v :V
   :V :v
   :w :W
   :W :w
   :x :X
   :X :x
   :y :Y
   :Y :y
   :z :Z
   :Z :z})

(defn reactive?
  [unit1 unit2]
  (= unit1
     (get opposite-unit-pairs unit2)))

(get opposite-unit-pairs :a)

(defn collect-unreacted-unit
  [acc-polymer unit]
  (if (reactive? (last acc-polymer) unit)
    (pop acc-polymer)
    (conj acc-polymer unit)))

(defn react
  [polymer]
  (reduce collect-unreacted-unit [] polymer))

(defn solve-part1 [polymer]
  (->> polymer
       react
       count))

(comment
  (->> input
       solve-part1))

;; 파트 2
; a-z까지 모든 A/a B/b 지워보면서 길이가 가장 짧은 Polymer를 만드는 것
; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.

(defn remove-given-unit-pair-in-polymer
  [given-unit polymer]
  (remove (fn [unit]
            (or (= unit given-unit)
                (= unit (get opposite-unit-pairs given-unit))))
          polymer))

(defn solve-part2 [polymer]
  (reduce (fn [shortest-polymer-cnt unit] (->> polymer
                                               (remove-given-unit-pair-in-polymer unit)
                                               react
                                               count
                                               (min shortest-polymer-cnt)))
          (count polymer)
          ascii-lowercases))

(comment
  ;; 4
  (->> (seq "dabAcCaCBAcCcaDA")
       solve-part2)

  ;; 6188
  (->> input
       solve-part2))