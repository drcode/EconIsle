(ns elon-tusk.engine-test
  (:require [elon-tusk.engine :refer :all]
            [fbc-utils.test :as te]))

(te/test  (calc-affordability [{:owned 0
                                :price 4}
                               {:owned 5
                                :price 2}
                               {:owned 5
                                :price 3}]
                              4)
          1
          (calc-affordability [{:owned 0
                                :price 4}
                               {:owned 5
                                :price 2}
                               {:owned 5
                                :price 3}]
                              38)
          7
          (calc-affordability [{:owned 0
                                :price 4}
                               {:owned 5
                                :price 2}
                               {:owned 5
                                :price 3}]
                              37)
          6
          (calc-affordability [{:owned 1
                                :price 4}
                               {:owned 5
                                :price 2}
                               {:owned 5
                                :price 3}]
                              34)
          7
          (calc-affordability [{:owned 1
                                :price 4}
                               {:owned 5
                                :price 2}
                               {:owned 5
                                :price 3}]
                              33)
          6)

