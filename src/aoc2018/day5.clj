(ns aoc2018.day5
  (:require [clojure.string :refer [trim lower-case upper-case]]))

;; 파트 1
;; 입력: dabAcCaCBAcCcaDA
;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA
;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.

(def input (trim (slurp "resources/aoc2018/day5.txt")))

(def ascii-lowercases (map char (range 97 123)))
(def ascii-uppercases (map char (range 65 91)))

(defn character->lower-case-character
  "입력된 문자를 소문자로 변환합니다."
  [c]
  (first (seq (lower-case c))))

(defn character->upper-case-character
  "입력된 문자를 대문자로 변환합니다."
  [c]
  (first (seq (upper-case c))))

(defn generate-opposite-units-map [f units]
  (reduce (fn [units-map unit] (assoc units-map unit
                                      (f unit)))
          {}
          units))

(def opposite-polarity-units-map
  "소->대, 대->소를 키 값으로 가지는 해시맵을 만듭니다.
   양극성을 나타낼 수 있는 맵
   "
  (merge (generate-opposite-units-map character->lower-case-character ascii-uppercases)
         (generate-opposite-units-map character->upper-case-character ascii-lowercases)))

(defn react
  [s]
  (reduce (fn [polymer unit] (cond
                               (= unit (get opposite-polarity-units-map (last polymer))) (pop polymer)
                               :else
                               (conj polymer unit))) [] s))

(defn solve-part1 [polymer]
  (->> polymer
       react
       count))

(comment

  ;; 97 ~ 122 ASCII Code는 lowercase 알파벳
  ascii-lowercases

  ;; 65 ~ 90 ASCII Code는 uppercase 알파벳
  ascii-uppercases

  (= (character->lower-case-character \B) \b)
  (= (character->upper-case-character \b) \B)
  (= (character->upper-case-character \B) \B)

  ;; 대 <-> 소 문자 변환
  opposite-polarity-units-map
  (get opposite-polarity-units-map \a)
  (get opposite-polarity-units-map \B)


  (->> (seq "dabAcCaCBAcCcaDA")
       solve-part1)

  (->> (seq input)
       solve-part1))

(defn remove-given-unit-pair-in-polymer [unit polymer]
  (remove (fn [u] (if (or (= u unit) ;; \a = \a
                          (= u (character->upper-case-character unit))) ;; \A = \a and \A = \A
                    true
                    false)) polymer))

;; 파트 2
; a-z까지 모든 A/a B/b 지워보면서 길이가 가장 짧은 Polymer를 만드는 것
; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.

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
  (->> (seq input)
       solve-part2))