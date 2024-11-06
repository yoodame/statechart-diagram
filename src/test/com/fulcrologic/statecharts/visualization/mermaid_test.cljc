(ns com.fulcrologic.statecharts.visualization.mermaid-test
  (:require
    [com.fulcrologic.statecharts.elements :as ele]
    [com.fulcrologic.statecharts.chart :refer [statechart]]
    [com.fulcrologic.statecharts :as sc]
    [fulcro-spec.core :refer [specification assertions =>]]
    [com.fulcrologic.statecharts.visualization.mermaid :as mm]))

(specification "Element labels"
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
        id->label    (mm/element-labels simple-chart)]

    (assertions
      "Always uses [*] for initial"
      (id->label :initial) => "[*]"
      "Uses the the pascal-cased keyword's name by default"
      (id->label :state/primary) => "Primary"

      "Transition labels are empty if there is no explicit label"
      (id->label :t1) => ""

      "Transition labels can be set using :diagram/label"
      (id->label :t2) => "empty?"

      "Uses :diagram/label if found"
      (id->label :state/other) => "Other State")))

(specification "State names in mermaid"
  (assertions
    "Are the stringified keyword (without the :)"
    (mm/state-name :initial) => "initial"
    (mm/state-name :a/b) => "a/b"
    (mm/state-name :foo.a/b) => "foo.a/b"))

(specification "Transition" :focus
  (let [chart (statechart {}
                (ele/initial {:id :initial} :state/primary)

                (ele/state {:id :state/primary}
                  (ele/transition {:id            :t2
                                   :diagram/label "empty?"
                                   :target        :state/other})
                  (ele/transition {:id            :t1
                                   :diagram/label "else"
                                   :target        :state/other}))

                (ele/state {:id            :state/other
                            :diagram/label "Other State"}))]
    (assertions
      "For a given parent"
      (mm/transitions-for chart :ROOT) => ["initial --> state/primary"
                                           "state/primary --> state/other"
                                           "state/primary --> state/other"])))