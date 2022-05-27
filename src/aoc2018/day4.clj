(ns aoc2018.day4
  (:require
   [clojure.string :refer [split-lines]]))

(def input (split-lines (slurp "resources/aoc2018/day4.txt")))

(def input-pattern #"\[(\d+)-(\d+)-(\d+)\s(\d+):(\d+)\]\s(.*)")

(def guard-shift-pattern #"Guard\s#(\d+)\sbegins\sshift")

(defn get-guard-id [description]
  (first (seq (rest (re-matches guard-shift-pattern description)))))

(defn list->map [[year month day hour minutes description]]
  {:year year :month month :day day :hour hour :minutes minutes :description description})

(defn sort-by-date-asc [l]
  (sort-by (juxt :year :month :day :hour :minutes) l))

(defn assoc-guard-fall-asleep-time [result guard-id wake-up-time asleep-time]

  (let [guard (keyword (str guard-id))
        guard-sleep-time (guard result)]
    (if (nil? guard-sleep-time)
      (assoc result guard (- (Integer/parseInt wake-up-time) (Integer/parseInt asleep-time)))
      (assoc result guard (+ guard-sleep-time (- (Integer/parseInt wake-up-time) (Integer/parseInt asleep-time)))))))

(defn solve-part1 [input]

  (loop [[info & rest-infos :as all] input
         current-guard-id nil
         fall-asleep-time 0
         result {}]

    (let [{:keys [minutes description]} info
          guard-id (get-guard-id description)]

      ;; (when-let [guard guard-id]
      ;;   (println "Guard ID: " guard result)
      ;;   (println "Guard ID: " current-guard-id result))

      (if (nil? rest-infos)
        result
        (cond
          (not (nil? guard-id)) (recur rest-infos guard-id fall-asleep-time result)
          (= "falls asleep" description) (recur rest-infos current-guard-id minutes result)
          (= "wakes up" description) (recur
                                      rest-infos
                                      current-guard-id
                                      0
                                      (assoc-guard-fall-asleep-time
                                       result
                                       current-guard-id
                                       minutes
                                       fall-asleep-time)))))))

(comment
  (let [working-histories (->> input
                               (map #(re-matches input-pattern %))
                               (map rest)
                               (map list->map)
                               (sort-by-date-asc))]


    (vals (solve-part1 working-histories))))