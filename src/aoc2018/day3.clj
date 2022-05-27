(ns aoc2018.day3
  (:require [clojure.set :refer [intersection]]
            [clojure.string :refer [split-lines]]))

(def input (split-lines (slurp "resources/aoc2018/day3.txt")))

;; 파싱을 위해서 정규표현식을 사용한다.
;; https://clojuredocs.org/clojure.core/re-matches

(defn number-list->key-map [[id x y w h]]
  {:id id :x x :y y :w w :h h})

(defn gen-tiles
  "w x h 크기의 격자를 만듭니다."
  [{:keys [x y w h]}] (for [i (range w) j (range h)] [(+ x i) (+ y j)]))


(->> input

     ;; 문자열 파싱  ["#50 @ 470,772: 12x12" "50" "470" "772" "12" "12"]
     ;; in: #1 @ 258,327: 19x22
     ;; out: ["#1 @ 258,327: 19x22" "1" "258" "327" "19" "22"]
     (map #(re-matches #"#(\d+)\s@\s(\d+),(\d+):\s(\d+)x(\d+)" %)) ; https://clojuredocs.org/clojure.core/re-matches

     ;; 정규표현식을 통해 찾은 문자열 리스트는 첫번 째에 원본이 있으므로 첫번째 것을 제외하고 가져옵니다.
     ;; out: ["1" "258" "327" "19" "22"]
     (map rest)

     ;; 문자열 리스트를 정수형 리스트로 변환합니다.
     ;; int 대신 java interop을 사용하였습니다.
     ;; out: [1 258 327 19 22]
     (map #(map (fn [n] (Integer/parseInt n)) %)) ; (https://clojuredocs.org/clojure.core/int)

     ;; out: {:id 1, :x 258, :y 327, :w 19, :h 22}
     (map number-list->key-map)

     ;; mapcat
     ;; out: ([258 327] [258 328] [258 329] [258 330] [258 331] [258 332] [258 333] ...)
     (mapcat gen-tiles)

     ;; out: {[51 754] 2, [851 85] 3, [647 191] 1, [212 414] 1, ...}
     frequencies
     (filter (fn [[_visited-tile count]] (> count 1))) ; 방문한 타일의 카운트가 1 이상이면 중복으로 방문된 타일이다.
     count)


(comment
  (re-matches #"#(\d+)\s@\s(\d+),(\d+):\s(\d+)x(\d+)" "#1 @ 258,327: 19x22") ;; ("1" "258" "327" "19" "22")
  (rest  ["#42 @ 675,276: 26x21" "42" "675" "276" "26" "21"])
  (number-list->key-map [1 258 327 19 22])

  (mapcat (fn [[k v]]
            (for [[k2 v2] v]
              (concat [k k2] v2)))
          '{:a {:x (1 2) :y (3 4)}
            :b {:x (1 2) :z (5 6)}}))


;; 파트 2
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)간을 채우면 아래와 같이 됨.

(def input-part2 (->> input
                      (map #(re-matches #"#(\d+)\s@\s(\d+),(\d+):\s(\d+)x(\d+)" %))
                      (map rest)
                      (map #(map (fn [n] (Integer/parseInt n)) %))
                      (map number-list->key-map)))

(defn find-nothing-common-in-tiles
  "각각 만든 타일맵을 전체 비교하여 "
  [t1 t2]
  (let [tiles-1 (set (gen-tiles t1)) ; 중복을 제거하고 방문한 격자목록만 남긴다.
        tiles-2 (set (gen-tiles t2))]
    (not (boolean (seq (clojure.set/intersection tiles-1 tiles-2)))))) ;; 서로 완전히 겹치지 않는다면 아무것도 안나와서 #{}이 나올 것이다.

(defn solve-part2 [input]
  (filter (fn [t1]
            (every? (fn [t2]
                      (or (= (:id t1) (:id t2)) ; 동일한 아이디는 건너뛴다.
                          (find-nothing-common-in-tiles t1 t2))) ; 다른 아이디인 경우는 타일을 만들어 비교해 교집합을 찾아내 겹치치 않는 영역이 있는지 찾는다.
                    input))
          input))


(comment
  (clojure.set/intersection #{:a :e :i :o :u}
                            #{:a :u :r}
                            #{:r :u :s})

  ;; seq로 변경을 해주는 이유는 boolean을 체크할때 #{} 이면 true이므로 seq로 변경하여 없는경우 nil이 나오도록 한다.
  (seq (clojure.set/intersection #{[51 754] [647 191]} #{[51 754] [647 191]}))
  (boolean (seq #{}))
  (boolean (seq #{}))
  (not (boolean (seq #{})))

  ;; input을 n * n으로 순회하면서 교차하는 부분이 없는 타일을 찾으면 된다.
  ;; n * n 은 찾는데 너무 오래걸린다..
  (->> input-part2
       solve-part2))