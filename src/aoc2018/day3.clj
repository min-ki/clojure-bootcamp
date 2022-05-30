(ns aoc2018.day3
  (:require [clojure.set :refer [intersection]]
            [clojure.string :refer [split-lines]]))

(def input (split-lines (slurp "resources/aoc2018/day3.txt")))

; \s -> space character class
(def input-pattern #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)")

;; patch: {:id, :x, :y, :w, :h} 를 키로 하는 맵이다.
(defn numbers->patch [[id x y w h]]
  {:id id :x x :y y :w w :h h})

;; coordinate: [x y] 형태의 벡터
(defn patch->coordinates
  "w * h 크기의 좌표집합을 만듭니다."
  [{:as _patch :keys [x y w h]}]
  (for [i (range w) j (range h)]
    [(+ x i) (+ y j)]))

(defn parse-patches [input]
  (->> input
       (map (partial re-matches input-pattern))
       (map rest)
       (map (fn [digits] (map (fn [digit] (Integer/parseInt digit)) digits)))
       (map numbers->patch)))

(defn count-duplicated-coordinates [coordinates]
  (->> coordinates
       frequencies
       (filter (fn [[_coordinate c]] (< 1 c)))
       count))

(defn solve-part1 [input]
  (->> input
       parse-patches
       (mapcat patch->coordinates)
       count-duplicated-coordinates))

(defn find-non-overlapping-coordinates
  [patch-1 patch-2]
  (let [coord-1 (set (patch->coordinates patch-1))
        coord-2 (set (patch->coordinates patch-2))]
    (nil? (seq (intersection coord-1 coord-2)))))

(defn solve-part2 [input]
  (let [patches (->> input
                     parse-patches)]
    (filter (fn [patch-1]
              (every? (fn [patch-2]
                        (or (= (:id patch-1) (:id patch-2))
                            (find-non-overlapping-coordinates patch-1 patch-2)))
                      patches))
            patches)))

(comment
  (->> input
       solve-part1)

  (->> input
       solve-part2))
