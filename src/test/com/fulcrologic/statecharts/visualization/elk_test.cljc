(ns com.fulcrologic.statecharts.visualization.elk-test
  (:require [fulcro-spec.core :refer [specification assertions =>]]
            [com.fulcrologic.statecharts.elements :as ele]
            [com.fulcrologic.statecharts.chart :refer [statechart]]
            [com.fulcrologic.statecharts.visualization.elk :as elk]
            [clojure.test :refer [deftest is testing]]))


(specification "chart to elk edges"
  (let [simple-chart (statechart {}
                       (ele/initial {:id :initial} :state/primary)

                       (ele/state {:id :state/primary}
                         (ele/transition {:id            :t2
                                          :cond          empty?
                                          :diagram/label "empty?"
                                          :target        :state/other})
                         (ele/transition {:id     :t1
                                          :target :state/other}))

                       (ele/state {:id            :state/other
                                   :diagram/label "Other State"}))
        chart->edges  (elk/chart->edges simple-chart)]

    (assertions
      "Has correct keys"
      (every? #(every? (partial contains? %) [:id :source :target :data]) chart->edges) => true

      "Has correct length"
      (count chart->edges) => 3)))

