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
               :layoutOptions {"elk.algorithm" "layered"},
               :children      [{:id "n1", :width 30, :height 30},
                               {:id "n2", :width 30, :height 30},
                               {:id "n3", :width 30, :height 30}],
               :edges         [{:id "e1", :sources ["n1"], :targets ["n2"]},
                               {:id "e2", :sources ["n1"], :targets ["n3"]}]}
        [layout set-layout!] (hooks/use-state nil)]
    (hooks/use-effect
      (fn []
        (async/go
          (let [lo (async/<! (elk/layout! graph))]
            (log/spy :info lo)
            (set-layout! lo)))
        js/undefined)
      [])
    (dom/div {:style {:width  "400px"
                      :height "400px"}}
      (dom/h2 "Graph")
      (when layout
        (elk/render-layout layout)))))

(ws/defcard elk-demo-card
  (ct.fulcro/fulcro-card
    {::ct.fulcro/wrap-root? false
     ::ct.fulcro/root       Root
     ::ct.fulcro/app        {}}))
