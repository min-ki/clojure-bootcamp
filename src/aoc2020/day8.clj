(ns aoc2020.day8
  (:require [clojure.string :refer [split-lines]]))

(def input (slurp "resources/aoc2020/day8.txt"))

(defn parse-instructions [raw-instructions]
  (->> (map (fn [instruction]
              (let [[operation argument] (clojure.string/split instruction #"\s")]
                [(keyword operation) (Integer/parseInt argument)]))
            raw-instructions)))

;; ============= part 1 ======================

(defn init-process-control-block [instructions]
  {:acc 0 :program-counter-addresses [] :instructions instructions :program-counter 0})

(defn increase-program-counter
  ([program-counter] (inc program-counter))
  ([program-counter operand] (+ program-counter operand)))

(comment
  (increase-program-counter 15 100)
  (increase-program-counter 30 -40))

(defn run-program
  "주어진 명령어를 수행합니다.
   
   acc: 누산기 (산술 논리결과가 저장되는 레지스터)
   program-counter-addresses: 지금까지 실행했던 program-counter 위치들
   instructions: 명령어들의 집합
   program-counter: 다음 명령이 실행될 위치
   "

  [{:keys [acc program-counter-addresses instructions program-counter] :as pcb}] ;; pcb = process control block

  (let [instruction (nth instructions program-counter) ;; 다음에 실행할 명령어
        opcode (first instruction)
        operand (second instruction)
        pcb (assoc pcb :program-counter-addresses (conj program-counter-addresses program-counter))]

    (println "[operation]" (name opcode) "operand: " operand "    pc: " program-counter  "acc: " acc)

    (case opcode
      :acc (assoc pcb :acc (+ acc operand) :program-counter (increase-program-counter program-counter))  ;; accumulate
      :jmp (assoc pcb :acc acc :program-counter (increase-program-counter program-counter operand)) ;; jump
      :nop (assoc pcb :acc acc :program-counter (increase-program-counter program-counter))))) ;; no operation


(defn duplicate-visited? [{:keys [program-counter-addresses program-counter]}]
  (nil? (seq (filter (fn [pc-address]
                       (= program-counter pc-address))
                     program-counter-addresses))))

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


;; ============= part 2 ======================

;; exactly one instruction is corrupted
;; 정확히 하나의 명령이 잘못되었다.

;; 프로그램 어딘가에 jmp가 nop이 되었거나 nop이 jmp가 되었다.
;; acc는 문제없다.

;; jmp -> nop
;; nop -> jmp로 바꿔서 프로그램이 종료하는지 확인

