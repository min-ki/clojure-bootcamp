(ns aoc2018.day4
  (:require
   [clojure.string :refer [split-lines]]))

(def input (split-lines (slurp "resources/aoc2018/day4.txt")))

(def input-pattern #"\[(\d+)-(\d+)-(\d+)\s(\d+):(\d+)\]\s(.*)")
(def guard-id-regex-pattern #"Guard #(\d+) begins shift")

(defn get-guard-id [action]
  (last (re-matches guard-id-regex-pattern action)))

(defn list->map [[year month day hour minutes action]]
  {:year year :month month :day day :hour hour :minutes minutes :action action})

(defn sort-by-date-asc [l]
  (sort-by (juxt :year :month :day :hour :minutes) l))

(defn get-repose-records [input]
  (->> input
       (map (partial re-matches input-pattern))
       (map rest)
       (map list->map)
       (sort-by-date-asc)))


(defn update-guard-sleep-time [result guard wake-up-time asleep-at]
  (-> result (update guard (fnil concat []) (range asleep-at wake-up-time))))

(defn get-guard-sleeps-info [repose-records]
  (->> repose-records
       (reduce (fn [[result current-guard asleep-at] repose-record]
                 (let [{:keys [minutes action]} repose-record
                       new-guard (get-guard-id action)
                       wakes-up (= "wakes up" action)
                       falls-asleep (= "falls asleep" action)]
                   (cond
                     new-guard [result new-guard nil] ;; 새로운 가드아이디를 설정해준다.
                     falls-asleep [result current-guard (Integer/parseInt minutes)] ;; 잠이든 시간을 넣어준다.
                     wakes-up [(update-guard-sleep-time result current-guard (Integer/parseInt minutes) asleep-at) current-guard nil])))
               [{} nil nil])
       first))


(defn solve-part1 [repose-records]
  (let [[guard-id sleeps-info]
        (->> repose-records
             get-guard-sleeps-info
             (sort-by (fn [[_guard_id mins]] (count mins)))
             last)
        chosen-minute (first (last (sort-by second (frequencies sleeps-info))))]
    (* (Integer/parseInt guard-id) chosen-minute)))


(defn solve-part2 [repose-records]
  (let [[guard-id [chosen-minute _chosen-frequency]]
        (->> repose-records
             get-guard-sleeps-info
             (map (fn [[guard-id mins]] [guard-id (last (sort-by second (frequencies mins)))]))
             (into {})
             (sort-by (fn [[_guard-id [_chosen-minute chosen-frequency]]] chosen-frequency))
             last)]
    (* (Integer/parseInt guard-id) chosen-minute)))

(comment
  (->> input
       get-repose-records
       solve-part1)

  (->> input
       get-repose-records
       solve-part2))




