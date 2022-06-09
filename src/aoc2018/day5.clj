(ns aoc2018.day5)

(def input
  (map (comp keyword str) (slurp "resources/aoc2018/day5.txt")))

(def units (->> (map char (range 97 123))
                (map str)
                (map keyword)))

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
  "두개의 unit을 입력받아 서로 반응하는지에 대한 여부를 반환합니다.
   Input: :x :X
   Output: true
   "
  [unit1 unit2]
  (= unit1
     (get opposite-unit-pairs unit2)))

(defn collect-unreacted-unit
  "폴리머와 주어진 유닛이 반응을한다면 마지막 유닛을 제거한 폴리머를 반환하고
   반응하지않는다면 폴리머 벡터에 유닛을 포함하여 반환합니다.
   Input: [:t :r :x ..]
   Output: [:x :r ..]
   "
  [acc-polymer unit]
  (if (reactive? (last acc-polymer) unit)
    (pop acc-polymer)
    (conj acc-polymer unit)))

(defn react
  "주어진 polymer를 화학 반응시킵니다. 결과물로 반응한 유닛들이 제거된 polymer를 반환합니다.
   Input: (:b :B :t :r :l :a ...)
   Output: [:d :a :C :B :A :c :a :D :A]
   "
  [polymer]
  (reduce collect-unreacted-unit [] polymer))

(defn solve-part1
  "입력으로 주어진 polymer를 반응시키고 난 후의 길이를 반환합니다.
   Input: (:b :B :t :r :l :a ...)
   Output: 10"
  [polymer]
  (->> polymer
       react
       count))

(defn remove-given-unit-pair-in-polymer
  "주어진 유닛을 제거한 폴리머를 반환합니다.
   Input: :b (:b :B :t :r ...)
   Output: [:B :t :r ...]
   "
  [given-unit polymer]
  (->> polymer
       (remove (fn [unit]
                 (or (= unit given-unit)
                     (reactive? unit given-unit))))
       (into [])))

(defn generate-one-unit-removed-polymers
  "폴리머를 입력받아 유닛리스트에서 하나의 유닛을 제거한 모든 폴리머들을 만들어냅니다.
   Input: (:b :B :t :r ...)
   Output: ([:t :r ...]
            [:a :A ...])
   "
  [polymer]
  (map #(remove-given-unit-pair-in-polymer % polymer) units))

(defn solve-part2
  "입력으로 주어진 폴리머에 대해서 모든 유닛을 하나씩 제거한 폴리머를 만들어 가장 짧은 폴리머를 찾아냅니다.
   
   Input (:b :B :t :r ...)
   Output: 6188
   "
  [polymer]
  (->> polymer
       generate-one-unit-removed-polymers
       (map react)
       (map count)
       sort
       first))

(comment
  (->> input
       solve-part1)

  (->> input
       solve-part2))