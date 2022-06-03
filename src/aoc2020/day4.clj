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

(def passport-required-fields #{"byr" "iyr" "eyr" "hgt" "ecl" "pid" "hcl"})

(defn parse
  "
   input: \"byr:1971\niyr:2017 hgt:160cm\neyr:2020 ecl:hzl\npid:1570962 ...\"
   output: (([\"byr\" \"1971\"] [\"iyr\" \"2017\"] [\"hgt\" \"160cm\"])
        ([\"byr\" \"1971\"] [\"iyr\" \"2017\"] [\"hgt\" \"160cm\"])
         ...)   
   "
  [input]
  (->> (split input #"\n\n")
       (map #(re-seq #"[^\s\n]+" %))
       (map (fn [passport]
              (map (fn [passport-content]
                     (split passport-content #":"))
                   passport)))))


(defn have-required-fields?
  "input: ([\"cid\" \"286\"] [\"hgt\" \"166cm\"] [\"byr\" \"1977\"] [\"iyr\" \"2012\"] [\"pid\" \"541909675\"] [\"ecl\" \"oth\"] [\"eyr\" \"2020\"] [\"hcl\" \"#59eb12\"])
   output: true or false
  "
  [passport-fields]
  (let [fields (set (map first passport-fields))]
    (nil? (seq (difference passport-required-fields fields)))))

(defn solve-part1
  "input: (([\"cid\" \"286\"] [\"hgt\" \"166cm\"] [\"byr\" \"1977\"] [\"iyr\" \"2012\"] [\"pid\" \"541909675\"] [\"ecl\" \"oth\"] [\"eyr\" \"2020\"] [\"hcl\" \"#59eb12\"])
         ([\"cid\" \"286\"] [\"hgt\" \"166cm\"] [\"byr\" \"1977\"] [\"iyr\" \"2012\"] [\"pid\" \"541909675\"] [\"ecl\" \"oth\"] [\"eyr\" \"2020\"] [\"hcl\" \"#59eb12\"])
         ...)
   output: 182
   "
  [passports]
  (->> passports
       (filter have-required-fields?)
       count))

(defn transform
  "input: [\"byr\" \"2012\"] | [\"hgt\" \"10cm\"]
   output: {:byr \"2012\"}    | {:hgt {:unit \"cm\" :scalar \"187\"}}
   "
  [passports]
  (map (fn [passport]
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
       passports))

(defn solve-part2 [passports]
  (->> passports
       (filter have-required-fields?)
       transform
       (filter #(s/valid? :passport/passport %))
       count))

(comment
  (->> input
       parse
       solve-part1)


  (->> input
       parse
       solve-part2))