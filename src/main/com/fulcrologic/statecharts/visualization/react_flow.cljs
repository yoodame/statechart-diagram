(ns com.fulcrologic.statecharts.visualization.react-flow
  (:require
    [com.fulcrologic.fulcro.algorithms.react-interop :refer [react-factory]]
    ["@xyflow/react" :refer [ReactFlow BaseEdge EdgeLabelRenderer getStraightPath useReactFlow]]))

(def ui-react-flow (react-factory ReactFlow))
(def ui-base-edge (react-factory BaseEdge))
(def ui-edge-label-renderer (react-factory EdgeLabelRenderer))

(defn use-react-flow [] (js->clj (useReactFlow)))
(defn get-straight-path [js-edge-props] (js->clj (getStraightPath js-edge-props)))
