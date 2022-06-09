(ns aoc2018.day6
  (:require [clojure.string :refer [split-lines
                                    split]]))

(def input (slurp "resources/aoc2018/day6.txt"))

(defn parse-vertex
  "N, M 형태의 문자열을 정수형 좌표 벡터로 변환한다.
   Input: \"301, 291\"
   Output: [301 291]
   "
  [s]
  (->> (split s #", ")
       (mapv #(Integer/parseInt %))))

(defn parse
  "input: 81, 157\n209, 355\n....
   output: ({:id 0 :vertex [10 20]} ...)"
  [s]
  (->> (split-lines s)
       (map-indexed
        (fn [index line]
          {:id index :vertex (parse-vertex line)}))))

(defn manhattan-distance
  "input: [[10 20] [30 40]]
   output: 40 (맨해튼 거리)"
  [[x1 y1] [x2 y2]]
  (+ (abs (- x2 x1))
     (abs (- y2 y1))))

(defn edges
  "4분면 양끝쪽에 해당하는 좌표집합"
  [x-start x-end y-start y-end]
  (concat (map (fn [y] [x-start y]) (range y-start y-end))
          (map (fn [y] [x-end y]) (range y-start y-end))
          (map (fn [x] [x y-start]) (range x-start x-end))
          (map (fn [x] [x y-end]) (range x-start x-end))))

(defn shortest-distance
  "구조분해로 첫번째, 두번째 길이의 데이터를 조회해서 둘이 길이가 같은지 체크한다.
   두개가 다르다면 가장 길이가 짧은 것이 하나만 존재한다는 것이다."
  [area vertexes]
  (let [[[id1 distance1] [_id2 distance2]]
        (->> vertexes
             (map (fn [{:keys [id vertex]}]
                    [id (manhattan-distance area vertex)]))
             (sort-by second))]
    (when (not= distance1 distance2)
      id1)))


(defn generate-areas
  [x-start x-end y-start y-end]
  (for [x (range x-start x-end)
        y (range y-start y-end)]
    [x y]))

(defn solve-part1 [vertexes]
  (let [coordinates (map :vertex vertexes)
        x-start (apply min (map first coordinates))
        y-start (apply min (map second coordinates))
        x-end (apply max (map first coordinates))
        y-end (apply max (map second coordinates))
        infinite-ids (into #{}  ;; to
                           (keep (fn [area]
                                   (shortest-distance area
                                                      vertexes)))  ;; xform - transducer
                           (edges x-start x-end y-start y-end))]  ;; from

    (->> (generate-areas x-start x-end y-start y-end)
         (keep (fn [area]
                 (shortest-distance area vertexes)))
         (remove infinite-ids)
         frequencies
         vals
         sort
         last)))

(defn safe-zone? [max-distances area vertexes]
  (let [total-distances (reduce +
                                (map #(max (manhattan-distance area (:vertex %)))
                                     vertexes))]
    (< total-distances max-distances)))

(def total-distances-less-than-10000? (partial safe-zone? 10000))

(defn solve-part2 [vertexes]
  (let [coordinates (map :vertex vertexes)
        x-start (apply min (map first coordinates))
        y-start (apply min (map second coordinates))
        x-end (apply max (map first coordinates))
        y-end (apply max (map second coordinates))
        areas (generate-areas x-start x-end y-start y-end)]

    (->> areas
         (filter #(total-distances-less-than-10000? % vertexes))
         count)))

(comment
  (require '[aoc2018.day6] :reload-all)
  (->> input
       parse
       solve-part1)

  (->> input
       parse
       solve-part2))
