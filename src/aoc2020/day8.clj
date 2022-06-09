(ns aoc2020.day8
  (:require [clojure.string :refer [split-lines]]))

(def input (slurp "resources/aoc2020/day8.txt"))

(defn parse-instructions
  "Input  : [\"jmp +265\" \"jmp +326\" \"acc +41\" ...]
   Output : ([:jmp 265], [:jmp 326], [:acc 41] ...)
   "
  [raw-instructions]
  (->> (mapv (fn [instruction]
               (let [[operation argument] (clojure.string/split instruction #"\s")]
                 [(keyword operation) (Integer/parseInt argument)]))
             raw-instructions)))

;; ============= part 1 ======================

(defn init-process-control-block
  "Input: ([:jmp 265], [:jmp 326], [:acc 41] ...)
   Output: {:acc 0
            :instruction-history []
            :instructions ([:jmp 265], [:jmp 326], [:acc 41] ...)
            :program-counter 0}"
  [instructions]
  {:acc 0 :instruction-history [] :instructions instructions :program-counter 0})

(comment
  (->> input
       split-lines
       parse-instructions
       init-process-control-block))


(defn increase-program-counter
  ([program-counter] (inc program-counter))
  ([program-counter operand] (+ program-counter operand)))

(comment
  (increase-program-counter 15)
  (increase-program-counter 30 -40))


(defmulti exec (fn [_process opcode _operand] opcode))

(defmethod exec :acc [{:keys [acc program-counter] :as process} _opcode operand]
  (assoc process
         :acc (+ acc operand)
         :program-counter (increase-program-counter program-counter)))

(defmethod exec :jmp [{:keys [program-counter] :as process} _opcode operand]
  (assoc process
         :program-counter (increase-program-counter program-counter operand)))

(defmethod exec :nop [{:keys [program-counter] :as process} _opcode _operand]
  (assoc process
         :program-counter (increase-program-counter program-counter)))

(defn run-program
  "주어진 명령어를 수행합니다.
   
   acc: 누산기 (산술 논리결과가 저장되는 레지스터)
   instruction-history: 지금까지 실행했던 program-counter 위치들
   instructions: 명령어들의 집합
   program-counter: 다음 명령이 실행될 위치
   "

  [{:keys [instruction-history instructions program-counter] :as process}]

  (let [instruction (nth instructions program-counter)
        [opcode operand] instruction
        process (assoc process :instruction-history (conj instruction-history program-counter))]
    (exec process opcode operand)))

(defn duplicate-visited? [{:keys [instruction-history program-counter]}]
  (empty? (filter (fn [pc-address]
                    (= program-counter pc-address))
                  instruction-history)))

(defn solve-part1 [input]
  (->> input
       split-lines
       parse-instructions
       init-process-control-block
       (iterate run-program)
       (take-while duplicate-visited?)
       last
       :acc))

(comment
  (solve-part1 input))
