(ns elon-tusk.elon-tusk
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [qlkit.core :as ql]
            [qlkit-renderer.core :as qr :refer-macros [defcomponent]]
            [goog.dom :refer [getElement]]
            [elon-tusk.parsers :as pa :refer [read mutate remote sync]]
            [qlkit-material-ui.core :refer [enable-material-ui!]]
            [cljs-http.client :as http :refer [post]]
            [cljs.reader :refer [read-string]]
            [cljs.pprint :refer [pprint]]
            [fbc-utils.core :as ut]
            [fbc-utils.style :as st]
            [fbc-utils.debug]))

(ut/devtools-night-mode)
(enable-console-print!)
(enable-material-ui!)

(def webcam-gap false)

(defonce app-state (atom (pa/new-day (pa/init-state))))

;; (pprint @app-state)

(defcomponent TodoItem
  (query [[:todo/text] [:db/id]])
  (render [{:keys [:todo/text] :as atts} state]
          [:list-item {:button true}
           [:list-item-text {:primary text}]
           [:list-item-secondary-action {}
            [:icon-button {}
             [:icon/cancel {:on-click (fn []
                                        (qr/transact! [:todo/delete!]))}]]]]))

(defcomponent Console
  (query [[:sim/log]])
  (render [{:keys [:sim/log] :as atts} state]
          [:div (st/style :p1000:bgrey:csilver:column-reverse {:width "35%" :overflow :hidden :font-size "1.5rem"})
           (for [item (reverse log)]
             [:div item])]))

(def island-pad-percent 10)

(def map-guy-width 50)
(def map-guy-height 135)
(def map-guy-width-percent 2)
(def map-guy-height-percent 6)

(defn pad-percent [n fudge]
  (str (float (- (+ island-pad-percent (/ (* n (- 100 (* 2 island-pad-percent))) 100)) fudge)) "%"))

(defcomponent Agent
  (query [[:agent/pos]])
  (render [{:keys [:agent/pos]
            :as   atts}
           state]
          (let [[x y] pos]
            [:div (st/style :absolute:imap_guy-png
                            {:width           "3%"
                             :height          "10%"
                             :background-size "100% 100%"
                             :z-index         (max 1 (int (- (* y 0.9) 5)))
                             :left            (pad-percent x (* 0.5 map-guy-width-percent))
                             :top             (pad-percent y map-guy-height-percent)})])))

(defcomponent BurgerGraph
  (query [[:graph/values]])
  (render [{:keys [:graph/values]
            :as   atts}
           state]
          (let [burgers (map :burgers values)
                peak    (apply max 1 burgers)]
            [:div 
             [:div (st/style :row
                             {:height "8rem"})
              [:div (st/style :itotal-burgers-png
                              {:width             "6%"
                               :margin            "0.5rem"
                               :background-position :center
                               :background-repeat :no-repeat
                               :background-size   :contain})]
              [:div {:width "94%"}
               [:div (st/style :start:end
                               {:height  "7rem"})
                (for [value burgers]
                  [:div (st/style :b777:cbbb:grow1
                                  {:height     (str (int (/ (* value 100) peak)) "%")
                                   :width      "2rem"
                                   :font-size  "0.5rem"
                                   :text-align "center"})
                   [:div {:margin-top "0.3rem"}
                    value]])]]]])))

;; (st/style :start:end {:height  "5rem" :display :flex})

(defcomponent PriceGraph
  (query [[:graph/values]])
  (render [{:keys [:graph/values]
            :as   atts}
           state]
          (let [peak    (apply max 1 (mapcat (fn [x]
                                               (vals (select-keys x #{:mammoth-price :bread-price :lettuce-price :ketchup-price}))) values))]
            [:div 
             [:div (st/style :row
                             {:height "7rem"})
              [:div (st/style :iprice-legend-png
                              {:width             "6%"
                               :margin            "2rem"
                               :margin-left "0.3rem"
                               :margin-right "1rem"
                               :background-position :center
                               :background-repeat :no-repeat
                               :background-size   :contain})]
              [:div {:width "94%"}
               [:div (st/style :end
                               {:height  "6rem"})
                (for [[value value-next] (map vector values (rest values))]
                  [:div (st/style :grow1
                                  {:height "6rem"
                                   :position :relative})
                   (for [[k v] (select-keys value #{:mammoth-price :bread-price :lettuce-price :ketchup-price})]
                     (let [y (fn [v]
                               (- 100 (/ (* v 100) peak)))
                           v-next (value-next k)]
                       [:div 
                        [:div {:background-color (case k
                                                   :mammoth-price :brown
                                                   :bread-price   :orange
                                                   :lettuce-price :green
                                                   :ketchup-price :red)
                               :transform-origin "top left"
                               :border-radius    "0.1rem"
                               :transform        (str "translateX(-0.5rem) scale(1.1) skewY(" (ut/atan2 (- (y v-next) (y v)) 35) "rad)")
                               :position         :absolute
                               :height           "0.2rem"
                               :width            "100%"
                               :top              (str (y v) "%")}]
                        (when (#{:mammoth-price :ketchup-price} k)
                          [:div {:margin-top "0.3rem"
                                 :font-size  "0.5rem"
                                 :position   :absolute
                                 :left       "0.5rem"
                                 :top              (str (+ 5 (y v-next)) "%")}
                           (str "$" (int v-next))])]))]
                  #_[:div (st/style :cblue:grow1
                                    {:height     (str (int (/ (* value 100) peak)) "%")
                                     :width      "2rem"
                                     :font-size  "0.4rem"
                                     :text-align "center"})
                     [:div (st/style :bblue
                                     {:height "0.2rem"})]
                     [:div {:margin-top "0.3rem"}
                      (int value)]])]
               [:div {:height "1rem"}]]]])))

(def dusk-length 10)

(defcomponent ElonTusk
  (component-will-unmount [{:keys [timer]
                            :as   state}]
                          (js/clearInterval timer)
                          (qr/update-state! assoc :timer nil))
  (query [[:elon-tusk/scenario-action] [:elon-tusk/scenario-text] [:elon-tusk/help] [:elon-tusk/time] [:elon-tusk/agents (ql/get-query Agent)] (ql/get-query Console) [:elon-tusk/graph (ql/get-query BurgerGraph)] [:elon-tusk/graph (ql/get-query PriceGraph)] [:elon-tusk/speed]])
  (render [{:keys [:elon-tusk/agents
                   :elon-tusk/graph
                   :elon-tusk/speed
                   :elon-tusk/time
                   :elon-tusk/help
                   :elon-tusk/scenario-text
                   :elon-tusk/scenario-action]
            :as   atts}
           {:keys [timer]
            :as   state}]
          (let [other-speed ({:slow :fast
                              :fast :slow} speed)
                brightness  (* 255 (/ (min dusk-length (- 50 (ut/abs (- 50 time)))) dusk-length))
                sun-color   (fn [n]
                              (int (* (+ (* 0.4 255) (* brightness 0.6)) n)))]
            [:div (st/style :column:bbbb 
                            {:height "100vh"})
             [:div (st/style :row:stretch {:height "66%"})
              [:div (st/style :relative {:width "65%"
                                         :background-color (let [] (str "rgb(" (sun-color 0.5) "," (sun-color 0.5) "," (sun-color 1) ")"))})
               [:div (st/style :absolute:imap_base-png
                               {:width           "100%"
                                :height          "100%"
                                :background-size "100% 100%"
                                :z-index         0})]
               (for [n (range 7)]
                 [:div (st/style :absolute
                                 {:width "100%"
                                  :height "100%"
                                  :background-size "100% 100%"
                                  :background-image (str "url('map_slice_" n ".png')")
                                  :z-index (+ 10 (* n 10))})])
               (for [agent agents]  
                 [Agent agent])
               (when scenario-action
                 [:div {:style {:backgroundColor "#AAF"
                                :position :absolute
                                :width     "50%"
                                :left      "25%"
                                :fontSize "1.2rem"
                                :padding   "0.5rem"
                                :bottom    "0.5rem"
                                :zIndex   1000
                                :textAlign     :center
                                :borderRadius "0.5rem"
                                :opacity 0.8}}
                  
                  "Current Scenario: " [:i (case scenario-action
                                             0 "Mammoth price fixed at $120"
                                             1 "Elon Tusk ketchup vacuum")]
                  [:br]
                  [:button {:variant    "outlined"
                            :size       "small"
                            :font-size  "0.8rem"
                            :margin-top "0.5rem"
                            :on-click (fn []
                                        (qr/transact! [:elon-tusk/scenario-end!]))}
                   "end"]])]
              [Console atts]]
             [:div {:height "34%"
                    :width  (if webcam-gap
                              "70%"
                              "100%")}
              [:button {:on-click (fn []
                                    (qr/update-state! assoc
                                                      :timer
                                                      (if timer
                                                        (js/clearInterval timer)
                                                        (js/setInterval (fn []
                                                                          (qr/transact! [:elon-tusk/tick! {}]))
                                                                        25))))}
               (if timer
                 "stop"
                 "start")]
              [:button {:on-click (fn []
                                    (qr/transact! [:elon-tusk/speed! {:val other-speed}]))}
               (name other-speed)]
              [:button {:on-click (fn []
                                    (qr/transact! [:elon-tusk/scenario!]))}
               "scenario"]
              [:button {:on-click (fn []
                                    (qr/transact! [:elon-tusk/help! {:val 0}]))}
               "help"]
              [PriceGraph graph]
              [:div {:height "0.5rem"}]
              [BurgerGraph graph]]
             (when (or help scenario-text)
               (list [:div (st/style :bgrey:absolute
                                     {:opacity 0.8
                                      :left   0
                                      :right  0
                                      :top    0
                                      :bottom 0
                                      :z-index 1000
                                      :on-click (fn []
                                                  (qr/transact! [:elon-tusk/help! {:val nil}]))})]
                     [:div (st/style :bwhite:absolute
                                     {:border-radius "1.5rem"
                                      :left "10%"
                                      :right (if webcam-gap
                                               "33%"
                                               "10%")
                                      :top "10%"
                                      :bottom "10%"
                                      :z-index 1000
                                      })
                      [:div {:position          :absolute
                             :top               "1rem"
                             :bottom            "1rem"
                             :left              "1rem"
                             :right             "1rem"
                             :background-image  (str "url("
                                                     (if help
                                                       (case help
                                                         0 "routine.png"
                                                         1 "burger.png"
                                                         2 "aptitudes.png"
                                                         3 "elon-tusk.png"
                                                         4 "money-king.png"
                                                         5 "prices.png")
                                                       (case scenario-text
                                                         0 "thinking-king.png"
                                                         1 "vacuum.png"))
                                                     ")")
                             :background-repeat :no-repeat
                             :background-size   :contain}]
                      [:button {:position :absolute
                                :bottom   "1rem"
                                :right    "1rem"
                                :on-click (fn []
                                            (if help
                                              (qr/transact! [:elon-tusk/help! {:val (mod (inc help) 6)}])
                                              (qr/transact! [:elon-tusk/scenario-start!])))}
                       (if help
                         "next"
                         "start")]]))])))

(defn remote-handler [query callback]
  (go (let [{:keys [status body] :as result} (<! (post "endpoint" {:edn-params query}))]
        (if (not= status 200)
          (print "server error: " body)
          (callback (read-string body))))))

(ql/mount {:component      ElonTusk
           :dom-element    (getElement "app")
           :state          app-state
           :remote-handler remote-handler
           :parsers        {:read   read
                            :mutate mutate
                            :remote remote
                            :sync   sync}})
