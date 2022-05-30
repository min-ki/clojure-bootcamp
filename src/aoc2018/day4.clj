(ns aoc2018.day4
  (:require
   [clojure.string :refer [split-lines]]))

(def input (split-lines (slurp "resources/aoc2018/day4.txt")))

(def input-pattern #"\[(\d+)-(\d+)-(\d+)\s(\d+):(\d+)\]\s(.*)")
(def guard-id-regex-pattern #"Guard #(\d+) begins shift")

(defn get-guard-id [description]
  (last (re-matches guard-id-regex-pattern description)))

(defn list->map [[year month day hour minutes description]]
  {:year year :month month :day day :hour hour :minutes minutes :description description})

(defn sort-by-date-asc [l]
  (sort-by (juxt :year :month :day :hour :minutes) l))

(defn get-repose-records [input]
  (->> input
       (map #(re-matches input-pattern %))
       (map rest)
       (map list->map)
       (sort-by-date-asc)))


(defn update-guard-sleep-time [result guard wake-up-time asleep-at]
  (-> result (update guard (fnil concat []) (range asleep-at wake-up-time))))

(defn get-guard-sleeps-info [repose-records]
  (->> repose-records
       (reduce (fn [[result current-guard asleep-at] repose-record]
                 (let [{:keys [minutes description]} repose-record
                       new-guard (get-guard-id description)
                       wakes-up (= "wakes up" description)
                       falls-asleep (= "falls asleep" description)]
                   (cond
                     new-guard [result new-guard nil] ;; 새로운 가드아이디를 설정해준다.
                     falls-asleep [result current-guard (Integer/parseInt minutes)] ;; 잠이든 시간을 넣어준다.
                     wakes-up [(update-guard-sleep-time result current-guard (Integer/parseInt minutes) asleep-at) current-guard nil])))
               [{} nil nil])
       first))


(defn solve-part1 [repose-records]
  (let [[guard-id sleeps-info] (->> repose-records
                                    get-guard-sleeps-info
                                    (sort-by (fn [[_guard_id mins]] (count mins)))
                                    (last)) ;; 이 것이 가장 많이 잠든 가드와 잠잔 시간

        ; 가장 빈번하게 잠든 가드가 어떤 시간에 가장 많이 자고 있었는지 찾는다.
        chosen-minute (first (last (sort-by second (frequencies sleeps-info))))]
    (* (Integer/parseInt guard-id) chosen-minute)))


(defn solve-part2 [repose-records]
  (let [[guard-id [chosen-minute _chosen-frequency]] (->> repose-records
                                                          get-guard-sleeps-info
                                                          (map (fn [[guard-id mins]] [guard-id (last (sort-by second (frequencies mins)))]))
                                                          (into {}) ; VectorList -> HashMap
                                                          (sort-by (fn [[_guard-id [_chosen-minute chosen-frequency]]] chosen-frequency))
                                                          (last))]
    (* (Integer/parseInt guard-id) chosen-minute)))

(comment
  (->> input
       get-repose-records
       solve-part1)

  (->> input
       get-repose-records
       solve-part2))




