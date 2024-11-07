(ns com.fulcrologic.statecharts.visualization.elk-cards
  (:require
    [com.fulcrologic.fulcro.react.hooks :as hooks]
    [com.fulcrologic.statecharts.visualization.elk :as elk]
    [clojure.core.async :as async]
    [com.fulcrologic.fulcro.components :refer [defsc]]
    [com.fulcrologic.fulcro.dom :as dom]
    [nubank.workspaces.card-types.fulcro3 :as ct.fulcro]
    [nubank.workspaces.core :as ws]
    [taoensso.timbre :as log]))


(comment
  (let [e     (new elk)
        graph (clj->js)]
    (-> e
      (.layout graph)
      (.then js/console.log)))

  )
(defsc Root [this props]
  {:query         [:ui/none]
   :initial-state {:ui/none 42}
   :use-hooks?    true}
  (let [graph {:id            "root",
               :layoutOptions {"elk.hierarchyHandling"                     "INCLUDE_CHILDREN",
                               "elk.algorithm"                             "layered",
                               "elk.layered.considerModelOrder"            "NODES_AND_EDGES",
                               "elk.layered.wrapping.strategy"             "MULTI_EDGE",
                               "elk.aspectRatio"                           "2",
                               "elk.direction"                             "RIGHT",
                               "elk.spacing.nodeNode"                      50 ; vertical
                               "elk.layered.spacing.nodeNodeBetweenLayers" 50} ; horizontal
               :children      [{:id "n1", :width 50, :height 50},
                               {:id "n2", :width 50, :height 50},
                               {:id       "n3", :width 50, :height 50
                                :children [{:id     "n4"
                                            :width  10
                                            :height 10}
                                           {:id     "n5"
                                            :width  10
                                            :height 10}
                                           {:id     "n6"
                                            :width  10
                                            :height 10}]}],
               :edges         [{:id "e1", :sources ["n1"], :targets ["n2"]},
                               {:id "e2", :sources ["n1"], :targets ["n3"]}
                               {:id "ie1" :sources ["n4"] :targets ["n5"]}
                               {:id "ie2" :sources ["n5"] :targets ["n6"]}
                               {:id "ie3" :sources ["n6"] :targets ["n1"]}]}
        [layout set-layout!] (hooks/use-state nil)]
    (hooks/use-effect
      (fn []
        (async/go
          (let [lo (async/<! (elk/layout! graph))]
            (log/spy :info lo)
            (set-layout! lo)))
        js/undefined)
      [])
    (dom/div {:style {:paddingTop "20px"
                      :width      "300px"
                      :height     "300px"}}
      (dom/h2 "Graph")
      (when layout
        (elk/render-layout layout)))))

(ws/defcard elk-demo-card
  (ct.fulcro/fulcro-card
    {::ct.fulcro/wrap-root? false
     ::ct.fulcro/root       Root
     ::ct.fulcro/app        {}}))
