(ns aoc2018.day1
  (:require [clojure.string :refer [split-lines]]))

(def input (->> (slurp "resources/aoc2018/day1.txt")
                split-lines
                (map #(Integer/parseInt %))))

(defn solve-part1-1 [input]
  (loop [[number & rest-numbers :as in] input
         out 0]
    (if (nil? in)
      out
      (recur rest-numbers
             (+ out number)))))

(defn solve-part1-2 [input]
  (reduce + 0 input))

(comment
  (->> input
       solve-part1-2)
  (->> input
       solve-part1-1))


(defn solve-part2-1 [input]
  (loop [[change-value & rest-change-values] input
         found-frequencies #{0}
         current-frequency 0]
    (let [resulting-frequency (+ current-frequency change-value)]
      (if (contains? found-frequencies resulting-frequency)
        resulting-frequency
        (recur rest-change-values
               (conj found-frequencies resulting-frequency)
               resulting-frequency)))))

(defn solve-part2-2 [input]
  (reduce (fn [{:keys [result found-frequencies] :as _acc}  current]
            (let [resulting-frequency (+ result current)]
              (if (contains? found-frequencies resulting-frequency)
                (reduced resulting-frequency)
                {:result resulting-frequency :found-frequencies (conj found-frequencies resulting-frequency)})))
          {:result 0 :found-frequencies #{0}}
          input))

; Refactoring 5/27
(defn find-first-duplicated [seen current]
  (if (contains? seen current)
    (reduced current)
    (conj seen current)))

(defn solve-part2-3 [input]
  (->> input
       cycle
       (reductions + 0)
       (reduce find-first-duplicated #{})))

(comment
  (->> input
       cycle ;; input을 무한시퀀스로 변경
       solve-part2-1)

  (->> input
       cycle
       solve-part2-2)

  (->> input
       solve-part2-3)

  (->> input
       cycle
       (reductions + 0) ;; 중간과정을 계속해서 볼 수 있는 함수
       (reduce find-first-duplicated #{})))

; [피드백 정리]
; 하나의 함수에서 하는일이 많다.
; 작은 함수로 나누어서 처리
; first는 클로저 코어 함수 이름이므로 사용하지 않는게 좋다.
; apply 함수
; 의도가 잘들어나도록 변수명 작성
