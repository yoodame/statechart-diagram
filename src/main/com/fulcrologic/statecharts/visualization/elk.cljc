(ns com.fulcrologic.statecharts.visualization.elk
  (:require
    #?@(:cljs [[com.fulcrologic.fulcro.dom :as dom]
               ["elkjs/lib/elk.bundled.js" :as elk]]
        :clj  [[com.fulcrologic.fulcro.dom-server :as dom]])
    [com.fulcrologic.fulcro.components :as comp]
    [clojure.string :as str]
    [clojure.core.async :as async]
    [taoensso.timbre :as log]))

(defn layout!
  "Returns an async channel on which the resulting layout will be returned"
  [graph]
  #?(:cljs
     (let [e     (new elk)
           c     (async/chan)
           graph (clj->js graph)]
       (-> e
         (.layout graph)
         (.then (fn [result]
                  (async/go
                    (async/>! c (js->clj result :keywordize-keys true))))))
       c)))

(defn render-node [{:keys [id x y width height] :as props}] (dom/rect (assoc props :key id)))
(defn render-edge [{:keys [id sections] :as props}]
  (let [paths (mapv
                (fn [{:keys [startPoint endPoint bendPoints]}]
                  (str/join " "
                    (flatten
                      [(str "M " (:x startPoint) " " (:y startPoint))
                       (mapv
                         (fn [{:keys [x y]}]
                           (str "L " x " " y))
                         bendPoints)
                       (str "L " (:x endPoint) " " (:y endPoint))])))
                sections)]
    (comp/fragment {}
      (mapv (fn [p] (dom/path {:key id :d p})) paths))))

(defn render-layout [{:keys [children edges]}]
  (dom/svg {:width 400 :height 400 :xmlns "http://www.w3.org/2000/svg"}
    (mapv render-node children)
    (mapv render-edge edges)))
