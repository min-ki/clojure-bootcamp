(ns aoc2020.day4
  (:require [clojure.set :as set :refer [difference]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str :refer [split]]))

(s/def :passport/cid string?)
(s/def :passport/byr #(<= 1920 (Integer/parseInt %) 2002))
(s/def :passport/iyr #(<= 2010 (Integer/parseInt %) 2020))
(s/def :passport/eyr #(<= 2020 (Integer/parseInt %) 2030))
(s/def :passport/hcl #(re-matches #"#[0-9a-f]{6}" %))
(s/def :passport/ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def :passport/pid #(re-matches #"\d{9}" %))
(s/def :passport/hgt (fn [{:keys [unit scalar]}]
                       (case unit
                         "cm" (<= 150 scalar 193)
                         "in" (<= 59 scalar 76)
                         false)))

(s/def :passport/passport
  (s/keys :req-un [:passport/byr
                   :passport/iyr
                   :passport/eyr
                   :passport/hgt
                   :passport/hcl
                   :passport/ecl
                   :passport/pid]
          :opt-un [:passport/cid]))

(def input (slurp "resources/aoc2020/day4.txt"))

(def passport-required-fields #{:byr :iyr :eyr :hgt :ecl :pid :hcl})

(defn split-passport-raw-data
  [input]
  (->> (split input #"\n\n")
       (map #(re-seq #"[^\s\n]+" %))
       (map (fn [passport]
              (map (fn [passport-content]
                     (split passport-content #":"))
                   passport)))))

(defn parse-fields
  "여권을 입력으로 받아 여권의 필드들을 파싱합니다."
  [passport]

  (reduce (fn [acc [key value]]
            (cond
              (and (= key "hgt")
                   (not (nil? (re-seq #"(\d+)cm" value)))) (assoc acc
                                                                  (keyword key)
                                                                  {:unit "cm"
                                                                   :scalar (Integer/parseInt (second (first (re-seq #"(\d+)cm" value))))})
              (and (= key "hgt")
                   (not (nil? (re-seq #"(\d+)in" value)))) (assoc acc
                                                                  (keyword key)
                                                                  {:unit "in"
                                                                   :scalar (Integer/parseInt (second (first (re-seq #"(\d+)in" value))))})
              :else (assoc acc (keyword key) value)))
          {}
          passport))

(defn parse-passport [passport-raw-seq]
  (->> passport-raw-seq
       split-passport-raw-data
       (map #(parse-fields %))))


(defn have-required-fields?
  [passport-fields]
  (let [fields (set (keys passport-fields))]
    (nil? (seq (difference passport-required-fields fields)))))


(defn solve-part1
  [passports]
  (->> passports
       (filter have-required-fields?)
       count))

(defn solve-part2
  [passports]
  (->> passports
       (filter have-required-fields?)
       (filter #(s/valid? :passport/passport %))
       count))

(comment
  (->> input
       parse-passport
       solve-part1)


  (->> input
       parse-passport
       solve-part2))
