(ns elon-tusk.engine
  #?(:cljs (:require-macros [snek.core :refer [defsnek defn]]
                            [fbc-utils.core :refer [forv]]))
  (:refer-clojure :rename {defn      core-defn
                           defmethod core-defmethod})
  (:require [fbc-utils.core :as ut]
            [fbc-utils.debug :as db]
            [clojure.pprint :as pp]
            [snek.core :as sn]
            #?(:clj [snek.core :refer [defsnek defn]])
            #?(:clj [fbc-utils.core :refer [forv]])))

(defsnek)

(def foods {:mammoth {:id    :mammoth
                      :ease  4
                      :order 0}
            :ketchup {:id    :ketchup
                      :ease  8
                      :order 1}
            :lettuce {:id    :lettuce
                      :ease  6
                      :order 2}
            :bread   {:id    :bread
                      :ease  6
                      :order 3}})

(def food-num (count foods))

(def stonage-word-1 [:saber :rock :arrow :mammoth :fur :sling])

(def stonage-word-2 [:skull :head :chest :foot :nose :arms])

(def stonage-full-names [:elon-tusk :king-fred :yakyak :ogg :biggo :berf :snuud :yikyik :rokko :yukko])

(def state-snek {:foods       {:_ {:id        :_
                                   :price     0
                                   :order-cur 0
                                   :order-num 0}}
                 :agents      {0 {:name  ""
                                  :id    0
                                  :money 0}}
                 :agent-foods {[0 :_] {:id           [0 :_]
                                       :productivity 0
                                       :owned        0}}
                 :orders      {[:_ 0] {:id       [:_ 0]
                                       :agent-id 0
                                       :amount 0}}})

(def log-atom (atom []))

(defn log [item]
  (swap! log-atom conj item))

(defn purge-log []
  (let [k @log-atom]
    (reset! log-atom [])
    k))

(def max-production 10)

(def money-supply-per-agent 1000)

(defn gen-agent []
  {:money money-supply-per-agent})

(def agents-num 40)

(defsnek -> state-snek)
(defn init-state []
  {:agents      (reduce ut/push-with-id
                        {}
                        (map (fn [agent nam]
                               (assoc agent :name nam))
                             (repeatedly agents-num gen-agent)
                             (concat (map name stonage-full-names)
                                     (repeatedly (fn []
                                                   (str (name (rand-nth stonage-word-1)) "-" (name (rand-nth stonage-word-2))))))))
   :agent-foods (into {}
                      (for [agent-id (range agents-num)
                            food     (keys foods)]
                        [[agent-id food] {:id           [agent-id food]
                                          :productivity (int (rand-int (int (* 2 (get-in foods [food :ease])))))
                                          :owned        0}]))
   :orders      {}
   :foods       (into {}
                      (for [food (keys foods)]
                        [food {:id        food
                               :price     150
                               :order-cur 0
                               :order-num 0}]))})

(defn target-formula [cost-others max-amount money max-price]
  (ut/ceil (/ (- (ut/sqrt (+ (* 4 max-amount cost-others max-price) (* 4 cost-others money) (ut/square max-price))) max-price) (* 2 cost-others))))

(defn target-burgers-optimum-formula ;;How many burgers can the agent expect to make, given owned food & money, assuming agent doesn't buy their most abundant food.
  [{:keys [foods]
    :as   state}
   agent-id]
  (let [ownership             (for [food (keys foods)]
                                [food (get-in state [:agent-foods [agent-id food] :owned])])
        [max-food max-amount] (apply max-key second ownership)
        max-price             (get-in state [:foods max-food :price])
        cost-others           (apply +
                                     (keep (fn [food]
                                             (when (not= food max-food)
                                               (get-in state [:foods food :price])))
                                           (keys foods)))
        starting-money        (get-in state [:agents agent-id :money])]
    (target-formula cost-others max-amount starting-money max-price)))

(defsnek [{:owned 0 :price 0}] 0 -> 0)
(defn calc-affordability [ownership money]
  (let [min-food-owned (:owned (apply min-key :owned ownership))]
    (loop [min-food-owned min-food-owned
           money          money]
      (let [next-min-food-owned (when-let [k (seq (filter (fn [{:keys [owned] :as food}]
                                                            (< min-food-owned owned))
                                                          ownership))]
                                  (apply min (map :owned k)))
            cost                (apply +
                                       (map :price
                                            (filter (fn [{:keys [owned] :as food}]
                                                      (>= min-food-owned owned))
                                                    ownership)))
            max-units           (int (/ money cost))
            max-level           (+ min-food-owned max-units)]
        (if (or (not next-min-food-owned) (>= next-min-food-owned max-level))
          max-level
          (recur next-min-food-owned (- money (* (- next-min-food-owned min-food-owned) cost))))))))

(defn target-burgers-as-much-as-affordable
  [{:keys [foods]
    :as   state}
   agent-id]
  (let [ownership (for [food-id (keys foods)]
                    {:food-id food-id
                     :owned   (get-in state [:agent-foods [agent-id food-id] :owned])
                     :price   (get-in state [:foods food-id :price])})
        money     (get-in state [:agents agent-id :money])]
    (calc-affordability ownership money)))

(defsnek state-snek {:_ nil} -> state-snek)
(defn work [{:keys [agents]
             :as   state}
            {:keys [vacuum]
             :as options}]
  (reduce (fn [{:keys [foods]
                :as   acc}
               {:keys [id
                       name]
                :as   item}]
            (let [[food produced] (apply max-key
                                         (fn [[food productivity]]
                                           (* productivity (get-in foods [food :price])))
                                         (forv [food (keys foods)]
                                               [food (if (and vacuum (= name "elon-tusk"))
                                                       (if (= food :ketchup)
                                                         150
                                                         1)
                                                       (get-in acc [:agent-foods [id food] :productivity]))]))
                  owned           (get-in acc [:agent-foods [id food] :owned])
                  amount          (+ owned produced)
                  acc             (assoc-in acc [:agent-foods [id food] :owned] amount)
                  burgers         (target-burgers-as-much-as-affordable acc id)
                  order-num       (get-in acc [:foods food :order-num])]
              (log {:cmd      :work
                    :agent-id id
                    :food     food
                    :produced produced})
              (-> acc
                  (assoc-in [:orders [food order-num]]
                            {:id       [food order-num]
                             :agent-id id
                             :amount   (max 0 (- amount burgers))})
                  (update-in [:foods food :order-num] inc))))
          state
          (shuffle (vals agents))))

(defsnek state-snek -> state-snek)
(defn eat [{:keys [agents]
            :as   state}]
  (reduce (fn [acc {:keys [id] ;;iterate through agents
                    :as   item}]
            (let [burgers (target-burgers-as-much-as-affordable state id)
                  acc     (reduce (fn [acc2 item2] ;;iterate through foods
                                    (let [goal (max 0 (- burgers (get-in acc [:agent-foods [id item2] :owned])))]
                                      (loop [{:keys [foods
                                                     agents
                                                     orders]
                                              :as   acc2} acc2
                                             goal         goal] ;;iterate through orders
                                        (if (pos? goal)
                                          (let [{:keys [order-cur
                                                        order-num
                                                        price]
                                                 :as   food} (foods item2)]
                                            (if (not= order-cur order-num)
                                              (let [{:keys          [amount]
                                                     other-agent-id :agent-id
                                                     :as            order} (orders [item2 order-cur])]
                                                (let [n    (min amount goal (int (/ (get-in acc2 [:agents id :money]) price)))
                                                      k    (* price n)
                                                      acc2 (-> acc2
                                                               (update-in [:agents id :money] - k)
                                                               (update-in [:agents other-agent-id :money] + k)
                                                               (update-in [:agent-foods [id item2] :owned] + n)
                                                               (update-in [:agent-foods [other-agent-id item2] :owned] - n))]
                                                  (log {:cmd       :sale
                                                        :food      item2
                                                        :amount    n
                                                        :money     k
                                                        :seller-id other-agent-id
                                                        :agent-id  id})
                                                  (if (= n amount) ;;will buy the full order
                                                    (recur (-> acc2
                                                               (update-in [:foods item2 :order-cur] inc)
                                                               (ut/dissoc-in [:orders [item2 order-cur]]))
                                                           (- goal n))
                                                    (update-in acc2 [:orders [item2 order-cur] :amount] - n))))
                                              acc2))
                                          acc2))))
                                  acc
                                  (keys foods))
                  burgers (apply min
                                 (for [food (keys foods)]
                                   (get-in acc [:agent-foods [id food] :owned])))]
              (log {:cmd      :eat
                    :agent-id id
                    :burgers  burgers})
              (reduce (fn [acc2 item2]
                        (update-in acc2 [:agent-foods [id item2] :owned] - burgers))
                      acc
                      (keys foods))))
          state
          (shuffle (vals agents))))

(defsnek state-snek {:_ nil} -> state-snek)
(defn sleep [{:keys [foods]
              :as   state}
             {:keys [mammoth-price]
              :as   options}]
  (-> (update state
              :agent-foods
              (fn [af]
                (into {}
                      (for [[k v] af]
                        [k  (update v :owned (fn [n] 0 #_(int (* n 0.75))))])))) 
      (assoc :orders {})
      (assoc :foods
             (sn/modify {:_ (fn [{:keys [price
                                         order-cur
                                         order-num
                                         id]
                                  :as   food}]
                              {:id        id
                               :order-cur 0
                               :order-num 0
                               :price     (if (and (= id :mammoth) mammoth-price)
                                            (min mammoth-price
                                                 (* price
                                                    (if (= order-cur order-num)
                                                      1.05
                                                      0.95)))
                                            (* price
                                               (if (= order-cur order-num)
                                                 1.05
                                                 0.95)))})}
                        foods))
      ))

(defsnek state-snek {:_ nil} -> state-snek)
(defn turn [state options]
  (-> state
      (work options)
      eat
      (sleep options)))

(defn money-total [{:keys [agents]
                    :as   state}]
  (apply + (map :money (vals agents))))

(defn foods-total [food
                   {:keys [agent-foods]
                    :as   state}]
  (apply + (map (fn [{:keys [id
                             owned]}]
                  (if (= (second id) food)
                    owned
                    0))
                (vals agent-foods))))

(defn main []
  (println "start")
  (let [k (iterate (fn [state]
                     (let [state (turn state {})
                           log (purge-log)]
                       (println "burgers " (apply + (keep :burgers log)))
                       state))
                   (init-state))
        states (take 60 k)]
    (doseq [state states]
      #_(pp/pprint (sn/query {:foods {:_ {:id     nil
                                          :price nil}}}
                             state))
      #_(println "money=" (money-total state))
      #_(println "burgers=" (burger-total state))
      #_(println "mammoth=" (foods-total :mammoth state))
      #_(println "ketchup=" (foods-total :ketchup state))
      #_(println "lettuce=" (foods-total :lettuce state))
      #_(println "bread=" (foods-total :bread state)))
    #_(last states)
    (pp/pprint (sn/query {:foods {:_ {:id     nil
                                      :price nil}}}
                         (last states)))
    nil))

;; (main)
