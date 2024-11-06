(ns com.fulcrologic.statecharts.visualization.mermaid
  (:require
    [clojure.string :as str]
    [camel-snake-kebab.core :refer [->PascalCaseString]]
    [com.fulcrologic.statecharts.chart :as chart]
    [com.fulcrologic.statecharts.malli-specs]
    [com.fulcrologic.statecharts :as sc]
    [com.fulcrologic.guardrails.malli.core :refer [>defn => ?]]
    [taoensso.timbre :as log]))

(defmulti element-label (fn [e] (:node-type e)))
(defmethod element-label :transition [{:keys [diagram/label]}]
  (or label ""))
(defmethod element-label :default [{:keys [initial? diagram/label id]}]
  (cond
    initial? "[*]"
    label label
    :else (->PascalCaseString id)))

(>defn element-labels
  "Given a statechart: Returns a map from element ID to the label that should be used in a diagram."
  [chart]
  [::sc/statechart => [:map-of :keyword :string]]
  (let [{::sc/keys [elements-by-id]} chart]
    (reduce-kv
      (fn [acc k v]
        (assoc acc k (element-label v)))
      {}
      elements-by-id)))

(>defn state-name [state-id]
  [:keyword => (? :string)]
  (str/join "/" (keep identity [(namespace state-id) (name state-id)])))

(>defn transitions-for [{::sc/keys [elements-by-id] :as chart} parent-id]
  [::sc/statechart :keyword => any?]
  (let [{:keys [children]} (if (= parent-id :ROOT) chart (get elements-by-id parent-id))
        transitions (reduce
                      (fn [acc node-id]
                        (let [{:keys [children]} (elements-by-id node-id)
                              transition-nodes (into []
                                                 (comp
                                                   (map elements-by-id)
                                                   (filter #(= :transition (:node-type %))))
                                                 children)]
                          (into acc
                            (mapcat
                              (fn [{:keys [target parent] :as node}]
                                (mapv
                                  (fn [t]
                                    (str (state-name parent) " --> " (state-name t))
                                    #_{:source parent
                                     :target t
                                     :label (element-label node)})
                                  target)))
                            transition-nodes)
                          ))
                      []
                      children)]
    transitions))
