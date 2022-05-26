(ns aoc2018.day2
  (:require
   [clojure.string :refer [split-lines]]
   [clojure.math.combinatorics]))

(defn read-file-str->vector
  "파일을 읽어 라인별로 분리하여 벡터로 반환합니다."
  [file] (split-lines (slurp file)))

(defn string->character-list
  "문자열을 character literal의 시퀀스를 반환합니다."
  [s] (seq s))

(defn character-list->frequency-count-map
  "문자 리스트를 빈도수를 세어 맵으로 반환합니다."
  [s] (frequencies s))

(defn extract-frequency-count-value->list
  "빈도수 맵에서 두번 째 값인 단어 카운트 수만 분리하여 리스트로 반환합니다."
  [l]
  (map second l))

;; contains?는 associative 관련에서 사용
;; 입력받은 current는 (2 3 4)와 같은 리스트형태
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


(def input (read-file-str->vector "resources/aoc2018/day2.txt"))

;; 리팩토링
(comment

  ;; threading last
  ;; day2 예제 input
  (->> input ;; 파일을 읽어 문자열 벡터로 변환한다.
       (map string->character-list) ;; 문자열 벡터를 순회하면서 각 문자열을 문자 리스트로 변환한다.
       (map character-list->frequency-count-map) ; 문자리스트를 빈도수 맵으로 변환한다.
       (map extract-frequency-count-value->list)
       (map #(distinct %))
       (map #(filter (fn [n] (> n 1)) %)) ; nested #()s are not allowed
       (reduce count-word-frequency {:twice-count 0 :triple-count 0})))

;; 1차 리팩토링
(let [duplicate-counts (->> input
                            (map frequencies)
                            (map extract-frequency-count-value->list)
                            (map distinct)
                            (flatten))]
  (* (count (filter #(= 2 %) duplicate-counts)) (count (filter #(= 3 %) duplicate-counts))))


  ;; 연오님이 알려주신 함수들
    ;; (->> '([l 2] [r 3] [k 1])
    ;;      (map second)
    ;;      distinct
    ;;      frequencies)

  ; 특정 조건 시 값 업데이트 하는 방법
    ;; (let [data '([l 2] [r 3] [k 1])
    ;;       result {:twice-count 0, :triple-count 0}
    ;;       numbers (set (map second data))]
    ;;   (cond (and (get numbers 2) (get numbers 3))
    ;;         (-> result
    ;;             (update :twice-count inc)
    ;;             (update :triple-count inc))
    ;;         (get numbers 2) (update result :twice-count inc)
    ;;         (get numbers 3) (update result :triple-count inc)
    ;;         :else result)))

  ;; Feedback
  ;; 문제를 복잡하게 풀기보다는 쉽게 해결하자
  ;; 2와 3을 카운팅하는 부분이 2와 3을 동시에 카운팅하려고하는데 이는 명확하게 분리된 것
  ;; x = y * z 처럼 y와 z를 각각 구해서 곱하는 방식으로 하면 문제가 더 쉬워질 수 있다.


;; (let [data (->> [1 2 3 4]
;;                 (map inc)
;;                 reverse)]
;;   [(first data) (last data)]) ; juxt

(vec "abcd")
(map (fn [c1 c2] (println c1 c2)) "abcd" "abcc")

;; (when (= 1 1) true)
;; when 절을 사용하면 동일한 문자는 내보내고 아닌경우 nil을 내보낼 수있다.
;; when 절은 주어진 평가식이 참이라면 body를 실행하고 아니면 nil을 반환한다.
(defn diff-one-character-exactly [[box-id1 box-id2]]
  (map (fn [c1 c2] (when (= c1 c2) c1)) box-id1 box-id2))

(->> input
     (#(clojure.math.combinatorics/combinations % 2))
     (map diff-one-character-exactly)
     (filter (fn [l] (= 1
                        (count (filter nil? l))))) ;; ((\w \l \k \i nil \g \s \q \y \f \e \c \j \q \q \m \n \x \a \k \t \d \r \h \b \z))
     first ;; (\w \l \k \i nil \g \s \q \y \f \e \c \j \q \q \m \n \x \a \k \t \d \r \h \b \z)
     (apply str)) ; wlkigsqyfecjqqmnxaktdrhbz

; apply str을 하면 리스트를 문자열로 변환가능.
; https://stackoverflow.com/questions/35884259/turning-a-list-of-strings-into-a-single-string-in-clojure

