(ns aoc2018.day6
  (:require [clojure.string :refer [split-lines]]))

; Day 6: Chronoal Coordinates
(def example ["1, 1", "1, 6" "8, 3" "3, 4" "5, 5" "8, 9"])
(def input (split-lines (slurp "resources/aoc2018/day6.txt")))

(defn find-edges-of-axises
  [vertexes]
  (let [xs (map first vertexes)
        ys (map second vertexes)]
    {:x-start (apply min xs)
     :x-end   (apply max xs)
     :y-start (apply min ys)
     :y-end   (apply max ys)}))

(defn generate-areas
  [{:keys [x-end y-end]}]
  (for [x (range (inc x-end))
        y (range (inc y-end))]
    [x y]))

(defn manhattan-distance
  [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1))
     (abs (- y2 y1))))

(defn parse [input]
  (->> input
       (map #(clojure.string/split % #", "))
       (map (fn [[x y]] [(Integer/parseInt x)
                         (Integer/parseInt y)]))))


;; edge영역을 가지는 area를 계산
(defn infinite-area
  [{:keys [x-start x-end y-start y-end]} [x y]]
  (or (<= x x-start)
      (>= x x-end)
      (<= y y-start)
      (>= y y-end)))


(defn vertex-distances [finite-areas vertexes]
  (let [distances (->> (for [area finite-areas
                             vertex vertexes]
                         {:vertex   vertex
                          :area     area
                          :distance (manhattan-distance vertex area)}))
        distances-to-vertexes (group-by :area distances)
        area-distance-group (group-by (juxt :area :distance) distances)]

    (reduce (fn [result area]
              (let [distances-to-vertexes (get distances-to-vertexes area)
                    shortest-distance (first (sort-by :distance distances-to-vertexes))
                    owned? (= 1 (count (get area-distance-group [area (:distance shortest-distance)])))
                    vertex-distances (get result (:vertex shortest-distance))]

                (if owned?
                  (if (nil? vertex-distances)
                    (conj result {(:vertex shortest-distance) [{:area     area
                                                                :vertex   (:vertex shortest-distance)
                                                                :distance (:distance shortest-distance)}]})
                    (->> (conj vertex-distances {:area     area
                                                 :vertex   (:vertex shortest-distance)
                                                 :distance (:distance shortest-distance)})
                         (assoc result (:vertex shortest-distance))))
                  result)))
            {}
            finite-areas)))

(comment
  (let [vertexes (parse input)
        edges (->> vertexes
                   find-edges-of-axises)
        areas (->> edges
                   generate-areas)
        inf-areas (->> areas
                       (filter (partial infinite-area edges)))
        finite-areas (remove (partial infinite-area edges) areas)
        vertex-distances (vertex-distances finite-areas vertexes)]
    (->>
     (map (fn [[k v]] [k (count v)]) vertex-distances)
     (sort-by second)
     last)))

;; 가장자리 위치 
;; Area는 locations (x, y)의 수 (가장 가까운)

;; 1. 좌표들의 최대, 최소 값을 찾는다. -> 가장자리 위치
;; 2. 주어진 제한 범위 안에서 모든 가능한 (x, y) locations를 찾는다.
;; 3. 각각의 coordinate 와 location에 대해서 모든 manhattan distance를 계산한다.
;; 4. 각 location에 대해서 가장 가까운 좌표를 결정한다.
;; 5. 각 coordinate에 대해서 가장 가까운 locations를 찾는다.

