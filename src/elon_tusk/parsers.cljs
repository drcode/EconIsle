(ns elon-tusk.parsers
  (:require-macros [fbc-utils.core :refer [defmethod-group]])
  (:require [qlkit.core :refer [parse-children parse-children-remote parse-children-sync]]
            [fbc-utils.core :as ut]
            [fbc-utils.debug]
            [clojure.string :as st]
            [elon-tusk.engine :as en]
            [snek.core :as sn]))

(sn/defsnek)

(def graph-item-snek {:burgers       0
                      :mammoth-price 0
                      :bread-price   0
                      :lettuce-price 0
                      :ketchup-price 0})

(def default-tick-increment 0.1)

(defn init-state []
  {:sim/log                   {}
   :elon-tusk/time            0
   :elon-tusk/tick-increment  default-tick-increment
   :elon-tusk/agents          (reduce (fn [acc item]
                                        (let [pos [(rand-int 100) (rand-int 100)]]
                                          (ut/push-with-id acc
                                                           {:agent/home pos
                                                            :agent/pos  pos})))
                                      {}
                                      (range en/agents-num))
   :elon-tusk/help            nil
   :elon-tusk/scenario-index  0
   :elon-tusk/scenario-text   nil
   :elon-tusk/scenario-action nil
   :elon-tusk/graph-queue     {}})

(def food-locations
  {:mammoth [15 20]
   :ketchup [85 80]
   :lettuce [15 80]
   :bread   [85 20]})

(defn fuzz-location [[x y]]
  [(+ x -5 (rand-int 10)) (+ y -5 (rand-int 10))])

(defonce engine-state (atom (en/init-state)))

(def turns-per-day 100) 

(def units-walked-per-day 200) ;;walking the length of the island is 100 units

(def graph-depth 40)

(defn key-min [db]
  (apply min (keys db)))

(defmulti read (fn [qterm & _] (first qterm)))

(defmethod read :qlkit-todo/todos
  [[dispatch-key params :as query-term] env {:keys [:todo/by-id] :as state}]
  (let [{:keys [todo-id]} params]
    (if todo-id
      [(parse-children query-term (assoc env :todo-id todo-id))]
      (for [id (keys by-id)]
        (parse-children query-term (assoc env :todo-id id))))))

(defmethod read :db/id
  [query-term {:keys [todo-id] :as env} state]
  (when (get-in state [:todo/by-id todo-id])
      todo-id))

(defmethod read :todo/text
  [query-term {:keys [todo-id] :as env} state]
  (get-in state [:todo/by-id todo-id :todo/text]))

(defmethod-group read [:elon-tusk/time]
  [[key :as query-term] env state]
  (state key))

(defmethod read :sim/log
  [query-term
   env
   {:keys [:sim/log]
    :as   state}]
  (map :text (sort-by :id (vals log))))

(defmethod-group read [:agent/pos]
  [[key _ :as query-term]
   {:keys [agent-id]
    :as   env}
   state]
  (get-in state [:elon-tusk/agents agent-id key]))

(defmethod-group read [:elon-tusk/help :elon-tusk/scenario-text :elon-tusk/scenario-action]
  [[key _ :as query-term]
   env
   state]
  (state key))

(defmethod read :elon-tusk/agents
  [query-term
   env
   {:keys [:elon-tusk/agents]
    :as   state}]
  (for [agent-id (keys agents)]
    (parse-children query-term (assoc env :agent-id agent-id))))

(defmethod read :graph/values
  [query-term
   {:keys [graph-type]
    :as   env}
   {:keys [:elon-tusk/graph-queue]
    :as   state}]
  (let [n (key-min graph-queue)]
    (for [id (range n (+ n graph-depth))]
      (or (graph-queue id) (sn/instance graph-item-snek)))))

(defmethod read :elon-tusk/graph
  [query-term env state]
  (parse-children query-term env))

(defmethod read :elon-tusk/speed
  [query-term env {:keys [:elon-tusk/tick-increment]
                   :as   state}]
  (if (= tick-increment default-tick-increment)
    :slow
    :fast))

(defmulti mutate (fn [qterm & _] (first qterm)))

(defmethod mutate :todo/new!
  [[dispatch-key params :as query-term] env state-atom]
  (let [{:keys [:db/id]} params]
    (swap! state-atom assoc-in [:todo/by-id id] params)))

(defmethod mutate :elon-tusk/speed!
  [[key params :as query-term] env state-atom]
  (swap! state-atom
         assoc
         :elon-tusk/tick-increment
         (if (= :fast (:val params))
           2
           default-tick-increment)))

(defmethod mutate :todo/delete!
  [query-term {:keys [todo-id] :as env} state-atom]
  (swap! state-atom update :todo/by-id dissoc todo-id))

(defmethod-group mutate [:elon-tusk/help!]
  [[key params :as query-term] env state-atom]
  (swap! state-atom assoc (ut/rem-exclamation key) (:val params)))

(defmethod mutate :elon-tusk/scenario!
  [query-term env state-atom]
  (swap! state-atom
         (fn [{:keys [:elon-tusk/scenario-index]
               :as   state}]
           (assoc state :elon-tusk/scenario-text scenario-index))))

(defmethod mutate :elon-tusk/scenario-start!
  [query-term env state-atom]
  (swap! state-atom
         (fn [{:keys [:elon-tusk/scenario-index]
               :as   state}]
           (-> state
               (assoc :elon-tusk/scenario-text nil)
               (assoc :elon-tusk/scenario-action scenario-index)))))

(defmethod mutate :elon-tusk/scenario-end!
  [query-term env state-atom]
  (swap! state-atom
         (fn [state]
           (-> state
               (update :elon-tusk/scenario-index inc)
               (assoc :elon-tusk/scenario-text nil)
               (assoc :elon-tusk/scenario-action nil)))))

(defn add-times [{:keys [:elon-tusk/daily-tasks
                         :elon-tusk/agents]
                  :as   state}]
  (reduce (fn [acc
               {:keys [id
                       :agent/task-num]
                :as   item}]
            (reduce (fn [acc2 [index tim :as item2]]
                      (assoc-in acc2 [:elon-tusk/daily-tasks [id index] :time] tim))
                    acc
                    (map-indexed vector (sort (repeatedly task-num #(rand-int 100))))))
          state
          (vals agents)))

(defn add-final-sleep [{:keys [:elon-tusk/agents]
                        :as   state}]
  (reduce (fn [acc
               {:keys [:agent/task-num
                       :agent/home
                       id]
                :as   item}]
            (-> acc
                (update-in [:elon-tusk/agents id :agent/task-num] inc)
                (assoc-in [:elon-tusk/daily-tasks [id task-num]]
                          {:id       [id task-num]
                           :cmd      :sleep
                           :pos      home
                           :agent-id id
                           :time     100})))
          state
          (vals agents)))

;; @engine-state

(defn push-with-id-queue [size db item]
  (let [db (ut/push-with-id db item)]
    (cond-> db
      (> (count db) size) (dissoc (key-min db)))))

(defn new-day [{:keys [:elon-tusk/agents
                       :elon-tusk/scenario-action]
                :as   state}]
  (swap! engine-state
         en/turn
         (case scenario-action
           nil {}
           0   {:mammoth-price 120}
           1   {:vacuum true}))
  (let [log     (remove (fn [{:keys [burgers
                                     amount]
                              :as   item}]
                          (or (zero? amount) (zero? burgers)))
                        (en/purge-log))
        burgers (apply + (keep :burgers log))
        state   (update state :elon-tusk/graph-queue (partial push-with-id-queue graph-depth)
                        {:burgers       burgers
                         :mammoth-price (get-in @engine-state [:foods :mammoth :price])
                         :bread-price   (get-in @engine-state [:foods :bread :price])
                         :lettuce-price (get-in @engine-state [:foods :lettuce :price])
                         :ketchup-price (get-in @engine-state [:foods :ketchup :price])})
        state   (assoc state :elon-tusk/daily-tasks {})
        state   (reduce (fn [acc
                             {:keys [cmd
                                     agent-id
                                     food
                                     produced]
                              :as   item}]
                          (let [n  (get-in acc [:elon-tusk/agents agent-id :agent/task-num])
                                id [agent-id n]]
                            (-> acc
                                (update-in [:elon-tusk/agents agent-id :agent/task-num] inc)
                                (assoc-in [:elon-tusk/daily-tasks id]
                                          (merge item
                                                 {:id  id
                                                  :pos (case cmd
                                                         :work (fuzz-location (food-locations food))
                                                         :sale (fuzz-location (let [[x y] (food-locations food)]
                                                                                [(ut/avg x 50) (ut/avg y 50)]))
                                                         :eat  (if (zero? (rand-int 2))
                                                                 [(+ 35 (rand-int 30)) (+ 15 (rand-int 70))]
                                                                 [(+ 15 (rand-int 70)) (+ 35 (rand-int 30))]))})))))
                        (sn/modify {:elon-tusk/agents {0 (fn [agent]
                                                           (merge agent
                                                                  {:agent/task-num 1
                                                                   :agent/task-cur 0}))}}
                                   (assoc state
                                          :elon-tusk/daily-tasks
                                          (into {}
                                                (for [agent-id (keys agents)]
                                                  [[agent-id 0] {:id       [agent-id 0]
                                                                 :cmd      :sleep
                                                                 :pos      (get-in state [:elon-tusk/agents agent-id :agent/home])
                                                                 :agent-id agent-id}]))))
                        log)]
    (-> state
        add-times
        add-final-sleep
        (assoc :elon-tusk/time 0))))

(def sim-log-depth 25)

(defn task-desc [nam
                 {:keys [cmd
                         agent-id
                         seller-id
                         money
                         amount
                         burgers
                         food]
                  :as   task}]
  (let [nam (apply str (interpose " " (map st/capitalize (st/split nam #"-"))))]
    (str (case cmd
           :eat (str nam " ate " burgers " burgers.")
           :sale (str nam " bought " amount " " (name food) ".")
           :sleep (str nam " is sleeping.")
           :work (str nam
                      " is "
                      (case food
                        :mammoth " hunting mammoth."
                        :bread   " farming bread."
                        :lettuce " farming lettuce."
                        :ketchup " gathering ketchup."))) #_(pr-str task))))

;; (apply str (interpose " " (map st/capitalize (st/split #"-" "foo-bar"))))

(defmethod mutate :elon-tusk/tick!
  [query-term env state-atom]
  (swap! state-atom
         (fn [{:keys [:elon-tusk/agents
                      :elon-tusk/time
                      :elon-tusk/tick-increment]
               :as   state}]
           (let [state                     (cond-> state
                                             (>= time 100) new-day)
                 {:keys [:elon-tusk/agents
                         :elon-tusk/time]} state]
             (-> state
                 ((partial reduce
                           (fn [acc
                                {:keys [id
                                        agent/pos
                                        agent/task-cur]
                                 :as   item}]
                             (let [[px py]          pos
                                   {goal      :pos
                                    task-time :time
                                    :as       task} (get-in acc [:elon-tusk/daily-tasks [id task-cur]])
                                   [gx gy]          goal
                                   units            (* (/ units-walked-per-day turns-per-day) tick-increment)
                                   dist             (ut/dist pos goal)
                                   frac             (if (zero? dist)
                                                      0
                                                      (/ units dist))
                                   new-task?        (>= time task-time)
                                   task-cur         (cond-> task-cur
                                                      new-task? inc)]
                               (assert goal)
                               (cond-> acc
                                 true      (update-in [:elon-tusk/agents id]
                                                      merge
                                                      {:agent/pos      (if (> units dist)
                                                                         goal
                                                                         [(+ px (* frac (- gx px))) (+ py (* frac (- gy py)))])
                                                       :agent/task-cur task-cur})
                                 new-task? (update :sim/log (partial push-with-id-queue sim-log-depth) {:text (task-desc (get-in @engine-state [:agents id :name]) (get-in acc [:elon-tusk/daily-tasks [id task-cur]]))})))))
                  (vals agents))
                 (update :elon-tusk/time + tick-increment))))))

(defmulti remote (fn [qterm & _] (first qterm)))

(defmethod remote :todo/new!
  [query-term state env]
  query-term)

(defmethod remote :todo/delete!
  [query-term state env]
  query-term)

(defmethod remote :todo/text
  [query-term state env]
  query-term)

(defmethod remote :db/id
  [query-term state env]
  query-term)

(defmethod remote :qlkit-todo/todos
  [query-term state env]
  (parse-children-remote query-term env)) 

(defmulti sync (fn [qterm & _] (first qterm)))

(defmethod sync :qlkit-todo/todos
  [[_ params :as query-term] result env state-atom]
  (for [{:keys [db/id] :as todo} result]
    (parse-children-sync query-term todo (assoc env :db/id id))))

(defmethod sync :todo/text
  [query-term result {:keys [:db/id] :as env} state-atom]
  (when id
    (swap! state-atom assoc-in [:todo/by-id id :todo/text] result)))

(defmethod sync :db/id
  [query-term result {:keys [:db/id] :as env} state-atom]
  (when id
    (swap! state-atom assoc-in [:todo/by-id id :db/id] result)))

(defmethod sync :todo/new!
  [query-term result env state-atom]
  (let [[temp-id permanent-id] result]
    (swap! state-atom
           update
           :todo/by-id
           (fn [by-id]
             (-> by-id
                 (dissoc temp-id)
                 (assoc permanent-id (assoc (by-id temp-id) :db/id permanent-id)))))))
