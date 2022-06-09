(ns aoc2018.day7
  (:require [clojure.set :refer [difference union subset?]]
            [clojure.string :refer [split-lines]]))

(def input (slurp "resources/aoc2018/day7.txt"))

(defn parse
  "Input: Step S must be finished before step G can begin.\nStep E must be finished before step T can begin.\nStep G must be finished before step A can begin.
   Output: [{:preceding-task S, :trailing-task G}
            {:preceding-task E, :trailing-task T}
            {:preceding-task G, :trailing-task A}
            {:preceding-task P, :trailing-task Z}]"
  [input]
  (->> input
       split-lines
       (map #(re-seq #"Step (\w) must be finished before step (\w) can begin." %))
       (map #(first %))
       (map #(rest %))
       (mapv (fn [[preceding-task trailing-task]]
               {:preceding-task (keyword preceding-task) :trailing-task (keyword trailing-task)}))))


(defn init-workers
  "작업에 사용할 작업자의 수를 입력받아 작업자 목록을 반환합니다.
   Input: 3   
   Output: [{:task nil :remain-time 0} {:task nil :remain-time 0} {:task nil :remain-time 0}]
   "
  [num-of-workers]
  (into [] (repeat num-of-workers {:task nil :remain-time 0})))

(defn calculate-task-time
  "주어진 작업의 작업시간을 계산하여 반환합니다.
   Input:  :A
   Output: 61
   "
  [task]
  (-> (int (first (name task)))
      (- (int \A))
      (+ 61)))

(defn task-rules->tasks
  "주어진 문제에서 다루는 모든 작업의 집합
   Input: [{:preceding-task S, :trailing-task G}
           {:preceding-task E, :trailing-task T}
           {:preceding-task G, :trailing-task A}
           {:preceding-task P, :trailing-task Z}
           {:preceding-task L, :trailing-task Z}
           {:preceding-task F, :trailing-task H}]
   
   Output: #{A B C D E F ...}
   "
  [task-rules]
  (union (set (map :preceding-task task-rules))
         (set (map :trailing-task task-rules))))


(defn tasks->trailing-tasks
  "선행작업을 기준으로 뒤에올수있는 작업들을 모아줍니다.
   Input: [{:preceding-task S, :trailing-task G}
           {:preceding-task F, :trailing-task H} ...]
   
   Output: {A #{B C}
            D #{F G}}
   "
  [tasks]
  (->> tasks
       (reduce (fn [acc {:keys [preceding-task trailing-task]}]
                 (assoc acc
                        trailing-task
                        (if (contains? acc trailing-task)
                          (conj (get acc trailing-task) preceding-task)
                          #{preceding-task})))
               {})))


(defn init-build-status
  "작업에 필요한 데이터들을 설정합니다.
   Input:
     - workers: [{:task nil :remain-time 0} {:task nil :remain-time 0} {:task nil :remain-time 0}]
     - works-rules:   [{:preceding-task S, :trailing-task G}
                 {:preceding-task E, :trailing-task T}
                 {:preceding-task G, :trailing-task A}
                 {:preceding-task P, :trailing-task Z}
                 {:preceding-task L, :trailing-task Z}
                 {:preceding-task F, :trailing-task H}] 

   Output: {:workers [{:task nil :remain-time 0}
                      {:task nil :remain-time 0}
                      {:task nil :remain-time 0}]
            :tasks  #{A B C D E F G H I J K L M N O P Q R S T U V W X Y Z}
            :task-time 0
            :finished-tasks #{}
            :assigned-tasks []
            :trailing-tasks {A #{D E G}, B #{A C E}}}
   "
  [workers taks-rules]
  {:workers workers
   :tasks (task-rules->tasks taks-rules)
   :task-time -1
   :finished-tasks #{}
   :assigned-tasks []
   :trailing-tasks (tasks->trailing-tasks taks-rules)})

(defn task-done-of-worker?
  "작업자의 작업이 완료되었는지 여부를 반환합니다.
   Input: {:task A :remain-time 0}
   Output: true"
  [worker]
  (and (= 0 (:remain-time worker))
       (not (nil? (:task worker)))))

(defn get-completed-worker-tasks
  "작업이 완료된 task 목록을 가져옵니다."
  [workers]
  (->> (filter task-done-of-worker? workers)
       (map :task)))

(defn decrease-task-remain-time
  "남은 작업시간을 1초 감소시켜줍니다.
   Input: {:task A :remain-time 1}
   Output: {:task A :remain-time 0}
   "
  [worker]
  {:task (:task worker)
   :remain-time (max (dec (:remain-time worker)) 0)})

(defn reset-task-end-of-worker
  "작업이 끝난 워커를 리셋합니다."
  [worker]
  (if (task-done-of-worker? worker)
    {:task nil :remain-time 0}
    worker))

(defn idle-worker?
  "작업자에 할당된 작업이 없어 쉬고있는 작업자인지 확인하여 불리언값을 반환합니다..
   Input: {:task A :remain-time 20} or {:task nil :remain-time 0}
   Output: false or true
   "
  [worker]
  (nil? (:task worker)))

(defn assign-next-tasks-to-idle-workers
  "작업자들에게 다음에 해야할 작업을 할당합니다.
   Input
     - workers: [{:task nil :remain-time 0} {:task nil :remain-time 0} {:task nil :remain-time 0}]
     - next-tasks: (A C)
     -  
   Output: {:newly-assigned-workers ({:task A :remain-time 12}, {:task B :remain-time 13} {:task nil :remain-time 0})
            :newly-assigned-tasks [A B]}
   "
  [workers next-tasks assigned-tasks]
  (let [idle-workers (filter idle-worker? workers)
        active-workers (filter (complement idle-worker?) workers)

        ;; 완료가 되지 않은 태스크가 남아있기 때문에 할당했던 태스크를 차집합 연산을 통해서 제거 후 사용합니다.
        unassigned-tasks (difference (set next-tasks) (set assigned-tasks))
        available-tasks (->> (take (count idle-workers) unassigned-tasks)
                             sort)]

    (if (empty? unassigned-tasks)
      {:newly-assigned-workers workers :newly-assigned-tasks assigned-tasks}
      (let [newly-assigned-workers (->> available-tasks
                                        (map (fn [task]
                                               {:task task :remain-time (calculate-task-time task)}))
                                        (concat (repeat (max (- (count idle-workers)
                                                                (count unassigned-tasks))
                                                             0) {:task nil :remain-time 0}))
                                        (concat active-workers))]
        {:newly-assigned-workers newly-assigned-workers :newly-assigned-tasks (vec (concat assigned-tasks
                                                                                           available-tasks))}))))


(defn renew-finished-tasks
  "새롭게 완료된 작업을 기존 완료된 작업과 병합하여 완료된 작업 목록을 갱신합니다."
  [{:keys [workers finished-tasks] :as build-status}]
  (assoc build-status :finished-tasks (->> (get-completed-worker-tasks workers)
                                           (into finished-tasks))))

(defn reset-finished-workers
  "작업이 끝난 워커들을 리셋하여 반환합니다."
  [{:keys [workers] :as build-status}]
  (assoc build-status :workers (map reset-task-end-of-worker workers)))

(defn find-next-tasks
  "다음에 해야할 작업 목록을 찾습니다.
   Input:
     - tasks: #{A B C ...}
     - trailing-tasks
     - finished-tasks #{A B C}
   Output: (A C)
 "
  [tasks trailing-tasks finished-tasks]
  (->> tasks
       (remove #(contains? finished-tasks %))
       (map (fn [task]
              {task (get trailing-tasks task)}))
       (reduce into {})
       (filter (fn [[_ preceding-tasks]]
                 (subset? preceding-tasks finished-tasks))) ;; 주어진 task의 선행작업들이 이미 완료되었는지 찾아봅니다.
       keys
       sort))

(defn assign-tasks-to-workers
  "다음 작업들을 작업자들에게 할당합니다."
  [{:keys [workers tasks finished-tasks trailing-tasks assigned-tasks] :as build-status}]
  (let [next-tasks (find-next-tasks tasks
                                    trailing-tasks
                                    finished-tasks)
        {:keys [newly-assigned-workers
                newly-assigned-tasks]} (assign-next-tasks-to-idle-workers workers
                                                                          next-tasks
                                                                          assigned-tasks)]
    (-> build-status
        (assoc :workers newly-assigned-workers)
        (assoc :assigned-tasks newly-assigned-tasks))))

(defn process-workers-task
  "작업자들의 태스크들을 한단계 진행하고 작업시간을 1 증가시켜줍니다."
  [{:keys [workers] :as build-status}]
  (-> build-status
      (assoc :workers (mapv decrease-task-remain-time workers))
      (update :task-time inc)))

;; 각 단계 업데이트 해주는 함수 별도 분리
(defn do-build-step
  "작업을 한차례 수행합니다.
   Input:
     - workers ({:task nil, :remain-time 0} {:task nil, :remain-time 0} .. n)
     - tasks #{A B C D ... Z}
     - finished-tasks #{}
   
   Output: {:workers [{:task nil, :remain-time 0} {:task nil, :remain-time 0} ...]
            :tasks #{A B C D E ...}
            :task-time 1000
            :finished-tasks #{A B C D ...}
            :assigned-tasks [F S D E ..]
            :trailing-tasks {A #{D E G L N O, B #{A C E K}}}}
   "
  [build-status]
  (->> build-status
       renew-finished-tasks     ;; STEP 1. 완료된 작업 목록을 갱신합니다.
       reset-finished-workers   ;; STEP 2. 완료된 작업자 목록을 리셋합니다.
       assign-tasks-to-workers  ;; STEP 3. 다음 작업들을 작업자들에게 할당합니다.
       process-workers-task))   ;; STEP 4. 작업자들의 태스크를 한단계 진행합니다.

(defn build
  "작업자들에게 작업을 할당하고 작업을 수행합니다.
   
   Input:
     task-rules: [{:preceding-step A :trailing-step B}
                    {:preceding-step A :trailing-step C}
                    {:preceding-step D :trailing-step E}]
     workers: [{:task nil, :remain-time 0} {:task nil, :remain-time 0} ...]
   
   Output: {:workers [{:task nil, :remain-time 0} {:task nil, :remain-time 0} ...]
            :tasks #{A B C D E ...}
            :task-time 1000
            :finished-tasks #{A B C D ...}
            :assigned-tasks [F S D E ..]
            :trailing-tasks {A #{D E G L N O, B #{A C E K}}}}
   "
  [task-rules workers]
  (->> (init-build-status workers task-rules)
       (iterate do-build-step)
       (drop-while (fn [{:keys [tasks finished-tasks]}]
                     (not (= tasks finished-tasks))))
       first))


(defn solve-part1
  "선행 후행 정보가 있는 작업을 받아서 작업의 순차적으로 처리되어야하는 작업의 순위를 문자열로 반환합니다.
   
   Input: [{:preceding-task :A :trailing-task :B}
           {:preceding-task :A :trailing-task :C}
           {:preceding-task :D :trailing-task :E}]
   Output: \"FSDEGPLJKNRYOQUAMIHTCVWZXB\"
   "
  [task-rules]
  (->> (build task-rules
              (init-workers 1))
       :assigned-tasks
       (map name)
       (apply str)))

(defn solve-part2
  "선행 후행 정보가 있는 작업을 받아서 5명의 작업자가 처리하는데 걸리는 시간을 반환합니다.
   
   Input: [{:preceding-task A :trailing-task B}
           {:preceding-task A :trailing-task C}
           {:preceding-task D :trailing-task E}]
   
   Output: 1000
   "
  [task-rules]
  (->> (build task-rules
              (init-workers 5))
       :task-time))

(comment
  (->> input
       parse
       solve-part1)

  (->> input
       parse
       solve-part2))