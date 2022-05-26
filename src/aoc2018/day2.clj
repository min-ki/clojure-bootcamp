(ns aoc2018.day2
  (:require
   [clojure.string :refer [split-lines]]))

;; Day2 - Inventory Management System

;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.

;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12

(defn read-file-str->vector
  "파일을 읽어 라인별로 분리하여 벡터로 반환합니다."
  [file] (split-lines (slurp file)))

;; (seq \"abc \") ;;=> (\a \b \c)
(defn string->character-list
  "문자열을 character literal의 시퀀스를 반환합니다."
  [s] (seq s))

(defn character-list->frequency-count-map
  "문자 리스트를 빈도수를 세어 맵으로 반환합니다."
  [s] (frequencies s))

(defn extract-frequency-count-value->list
  "빈도수 맵에서 두번 째 값인 단어 카운트 수만 분리하여 리스트로 반환합니다."
  [l]
  (map #(second %) l))

;; contains?는 associative 관련에서 사용
;; 입력받은 current는 (2 3 4)와 같은 리스트형태
;; 질문 .contains vs contains?
(defn count-word-frequency
  "'((2 3) (3) (2))와 같은 중복된 문자 카운트 목록을 인자로 받아 2와 3이 얼마나 나타나는지 카운트 합니다."
  [{:keys [twice-count triple-count]} duplication-counts]
  (let [result {:twice-count twice-count :triple-count triple-count}]
    (cond
      (and (.contains duplication-counts 2) (.contains duplication-counts 3)) (-> result
                                                                                  (update :twice-count inc)
                                                                                  (update :triple-count inc))
      (.contains duplication-counts 2) (update result :twice-count inc)
      (.contains duplication-counts 3) (update result :triple-count inc)
      :else result)))


(comment

  ;; threading last
  ;; day2 part-1
  (->> (read-file-str->vector "resources/aoc2018/day2.txt") ;; 파일을 읽어 문자열 벡터로 변환한다.
       (map string->character-list) ;; 문자열 벡터를 순회하면서 각 문자열을 문자 리스트로 변환한다.
       (map character-list->frequency-count-map) ; 문자리스트를 빈도수 맵으로 변환한다.
       (map extract-frequency-count-value->list)
       (map #(distinct %))
       (map #(filter (fn [n] (> n 1)) %)) ; nested #()s are not allowed
       (reduce count-word-frequency {:twice-count 0 :triple-count 0}))
       ; 7776

  ;; 연오님이 알려주신 함수들
  (->> '([l 2] [r 3] [k 1])
       (map second)
       distinct
       frequencies)

  ; 특정 조건 시 값 업데이트 하는 방법
  (let [data '([l 2] [r 3] [k 1])
        result {:twice-count 0, :triple-count 0}
        numbers (set (map second data))]
    (cond (and (get numbers 2) (get numbers 3))
          (-> result
              (update :twice-count inc)
              (update :triple-count inc))
          (get numbers 2) (update result :twice-count inc)
          (get numbers 3) (update result :triple-count inc)
          :else result))

  ;; Feedback
  ;; 문제를 복잡하게 풀기보다는 쉽게 해결하자
  ;; 2와 3을 카운팅하는 부분이 2와 3을 동시에 카운팅하려고하는데 이는 명확하게 분리된 것
  ;; x = y * z 처럼 y와 z를 각각 구해서 곱하는 방식으로 하면 문제가 더 쉬워질 수 있다.
  )




;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.

(defn diff-one-character-exactly?
  "정확히 한개의 문자가 다른지 체크하는 함수"
  [box-id1 box-id2]
  (let [word-diffs-by-index (map (fn [x y] (= x y))
                                 (vec (seq box-id1)) (vec (seq box-id2)))]
    (->> word-diffs-by-index
         (filter #(= false %))
         count
         (= 1))))

(diff-one-character-exactly? "fghij" "fguij")

(for [a [1 2 3]
      b [4 5 6]
      :when (odd? a)]
  [a b])


(comment

  (->> (read-file-str->vector "resources/aoc2018/day2.txt"))

  ;; 중첩 for문은 어떻게 만드는걸까?
  )

;; (defn check-correct-box-ids-between [box-id1 box-id2]

;;   (seq box-id1)
;;   (seq box-id2)
;;   )

;; (defn find-letters-common-between-two-correct-box-ids [boxes]
;;   (loop [[box-id & rest-boxs] (boxes)
;;          common-letters #{} ; 문자가 하나 다른 두개의 박스 아이디
;;          ]


;;       (if (complement (nil? common-letters))
;;         "abcd" ; 공통인 문자를 반환한다.
;;         (recur rest-boxs
;;                (if (check-correct-box-ids-between) common-letters
;;                ))))

;; (->> (read-file-str->vector "resources/aoc2018/day2.txt")
;;       (map string->character-list)
;;       (map vec)

;; ((->> (read-file-str->vector "resources/aoc2018/day2.txt")
;;       (map string->character-list)
;;       (map vec)

      ;; #_(map (filter #(= (count (filter (fn [n] (= false n)) %)) 1)))))


; 
