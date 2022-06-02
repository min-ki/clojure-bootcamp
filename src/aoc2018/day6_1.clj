(ns aoc2018.day6_1
  (:require [clojure.string :refer [split-lines
                                    split]]))

(def input (slurp "resources/aoc2018/day6.txt"))

(defn parse [s]
  (->> (split-lines s)
       (map-indexed (fn [index vertex]
                      {:id index :vertex  (as-> vertex v
                                            (split v #", ")
                                            (mapv #(Integer/parseInt %) v))}))))

(defn manhattan-distance
  [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1))
     (abs (- y2 y1))))

(defn edges
  "4분면 양끝쪽에 해당하는 좌표집합"
  [x-end y-end]
  (concat (map (fn [y] [0 y]) (range y-end))
          (map (fn [y] [x-end y]) (range y-end))
          (map (fn [x] [x 0]) (range x-end))
          (map (fn [x] [x y-end]) (range x-end))))

(defn shortest-distance
  "구조분해로 첫번째, 두번째 길이의 데이터를 조회해서 둘이 길이가 같은지 체크한다.
   두개가 다르다면 가장 길이가 짧은 것이 하나만 존재한다는 것이다."
  [area vertexes]

  (let [[[id1 distance1] [_id2 distance2]] (->> vertexes
                                                (map (fn [{:keys [id vertex]}]
                                                       [id (manhattan-distance area vertex)]))
                                                (sort-by second))]
    (when-not (= distance1 distance2)
      id1)))

(defn generate-areas
  [x-end y-end]
  (for [x (range x-end)
        y (range y-end)]
    [x y]))

(defn solve-part1 [vertexes]
  (let [coordinates (map :vertex vertexes)
        x-end (apply max (map first coordinates))
        y-end (apply max (map second coordinates))
        infinite-ids (into #{}  ;; to
                           (keep #(shortest-distance % vertexes))  ;; xform - transducer
                           (edges x-end y-end))]  ;; from

    (->> (generate-areas x-end y-end)
         (keep (fn [area]
                 (shortest-distance area vertexes)))
         (remove infinite-ids)
         frequencies
         vals
         sort
         last)))

(comment
  (into #{}
        (keep #(shortest-distance % '({:id 0, :vertex [81 157]}
                                      {:id 1, :vertex [209 355]}
                                      {:id 2, :vertex [111 78]}
                                      {:id 3, :vertex [179 211]})))
        [[81 157] [209 355]])

  (->> input
       parse
       solve-part1))


(defn safe-zone? [max-size-of-the-region area vertexes]
  (let [total-distances (reduce +
                                (map #(max (manhattan-distance area (:vertex %)))
                                     vertexes))]
    (< total-distances max-size-of-the-region)))

(def total-distances-less-than-10000? (partial safe-zone? 10000))

(defn solve-part2 [vertexes]
  (let [coordinates (map :vertex vertexes)
        x-end (apply max (map first coordinates))
        y-end (apply max (map second coordinates))
        areas (generate-areas x-end y-end)]

    (->> areas
         (filter #(total-distances-less-than-10000? % vertexes))
         count)))

(comment
  (->> input
       parse
       solve-part2))