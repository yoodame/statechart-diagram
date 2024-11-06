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

(def ^:dynamic *indent* 0)
(defn indent [] (str/join "" (repeat *indent* " ")))

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
                                  (fn [t] (str (indent)
                                            (if (get-in elements-by-id [parent :initial?])
                                              "[*]"
                                              (state-name parent))
                                            " --> "
                                            (if (= :final (get-in elements-by-id [t :node-type]))
                                              "[*]"
                                              (state-name t))))
                                  target)))
                            transition-nodes)
                          ))
                      []
                      children)]
    transitions))

(defmulti render (fn [chart ele] (:node-type ele)))

(defn- render-composite [{::sc/keys [elements-by-id] :as chart} {:keys [id children]}]
  (let [nodes  (mapv elements-by-id children)
        states (filterv #(#{:state :parallel} (:node-type %)) nodes)]
    (str/join "\n"
      (concat
        ;; State declarations with labels
        (keep (fn [{:keys [id initial?] :as state}]
                (when-not initial?
                  (str (indent) (state-name id) ": " (element-label state))))
          states)
        ;; transitions
        (transitions-for chart id)
        ;; emit the nested states/parallel elemements recursively
        (map (fn [s] (render chart s)) states)))))

(defmethod render :statechart [{::sc/keys [elements-by-id] :as chart} {:keys [id children] :as node}]
  (binding [*indent* (+ 3 *indent*)]
    (str "stateDiagram-v2\n"
      (render-composite chart node))))

(defmethod render :parallel [{::sc/keys [elements-by-id] :as chart} {:keys [id children]}]
  (let [nodes  (mapv elements-by-id children)
        states (filterv #(= :state (:node-type %)) nodes)]
    (str (indent) "state " (state-name id) " {\n"
      (binding [*indent* (+ 3 *indent*)]
        (str/join (str "\n" (indent) "--\n")
          (map (fn [state] (render chart state)) states)))
      "\n"(indent) "}")))

(defmethod render :default [{::sc/keys [elements-by-id] :as chart} {:keys [id children]}] (str "FIXME"))

(defmethod render :state [{::sc/keys [elements-by-id] :as chart} {:keys [id children] :as node}]
  (let [nodes   (mapv elements-by-id children)
        states  (filterv #(= :state (:node-type %)) nodes)
        atomic? (empty? states)]
    (when-not atomic?
      (str (indent) "state " (state-name id) " {\n"
        (binding [*indent* (+ *indent* 3)]
          (render-composite chart node))
        (indent) "}"))))
