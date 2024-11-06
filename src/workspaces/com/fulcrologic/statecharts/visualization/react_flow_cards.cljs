(ns com.fulcrologic.statecharts.visualization.react-flow-cards
  (:require
    [com.fulcrologic.fulcro.components :refer [defsc]]
    [com.fulcrologic.fulcro.dom :as dom]
    [com.fulcrologic.statecharts.visualization.react-flow :as rf]
    [nubank.workspaces.card-types.fulcro3 :as ct.fulcro]
    [nubank.workspaces.core :as ws]))

(defsc Root [this props]
  {:query         [:ui/none]
   :initial-state {:ui/none 42}}
  (dom/div {:style {:width "400px"
                    :height "400px"}}
    (rf/ui-react-flow {:nodes [{:id "1", :position {:x 0, :y 0}, :data {:label "1"}},
                               {:id "2", :position {:x 300, :y 0}, :data {:label "2"}}
                               {:id "3", :position {:x 100, :y 100}, :data {:label "3"}},]
                       :edges [{:id "e1-2", :type "step" :style {:markerStart "url(#arrowhead)"
                                                                 :markerEnd "url(#arrowhead)"} :source "1", :target "2"}
                               {:id "e1-3", :type "step" :source "1", :target "3"}]})))

(ws/defcard hook-demo-card
  (ct.fulcro/fulcro-card
    {::ct.fulcro/wrap-root? false
     ::ct.fulcro/root       Root
     ::ct.fulcro/app        {}}))
