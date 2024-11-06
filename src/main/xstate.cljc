(ns xstate
  "ALPHA. Conversion of this library's state charts into a js notation that can
  be used for playing with and diagramming using XState tools."
  (:require
   [camel-snake-kebab.core :as csk]
   [com.fulcrologic.statecharts :as sc]
   [com.fulcrologic.statecharts.chart :as chart]
   [dataico.subscriptions.statechart :refer [subscription-management]]
   [taoensso.timbre :as log]
   [clojure.string :as str]))

(defn jssym [n]
  (when n
    (let [base  (name (symbol n))
          alt   (if (str/ends-with? base "?")
                  (str "is-" base)
                  base)
          final (some-> alt
                        (csk/->camelCase)
                        (str/replace #"[^-a-zA-Z0-9_]" ""))]
      final)))

(defn collapse-initial-state
  "If `s` (element or id) is an initial state on `c` that contains only a simple transition, then
  return that target state's id. Otherwise returns s."
  [c s]
  (if (chart/initial? c s)
    (let [{:keys [children]} (chart/element c s)
          {:keys [target] :as t} (first children)]
      (if (and (= 1 (count children)) (= 1 (count target)))
        (first target)
        s))
    s))

(defn chart-to-tree [chart {:keys [children] :as node}]
  (let [child-elements (mapv (fn [child-id]
                               (chart-to-tree chart (chart/element chart child-id)))
                             children)]
    (-> node
        (dissoc ::sc/elements-by-id)
        (dissoc ::sc/ids-in-document-order)
        (assoc :children child-elements))))

(defn condition-symbol [{condition :cond
                         :keys     [event target] :as t}]
  (let [c (or (:diagram/condition t) (when condition 'unlabled))]
    (when c
      (jssym c))))

(defn actions [chart candidates]
  (vec
   (keep
    (fn [c]
      (let [{:keys [diagram/label diagram/expression event expr node-type]} (chart/element chart c)]
        (case node-type
          :script (or label expression (str "script"))
          :raise (str "raise('" (name event) "')")
          :send (str "send('" (name event) "')")
          nil)))
    candidates)))

(defn transitions [c state]
  (let [transitions-by-event (group-by :event
                                       (map #(chart/element c %) (chart/transitions c state)))
        event-map            (reduce-kv
                              (fn [acc event ts]
                                (assoc acc (if event (name event) "")
                                       (vec
                                        (keep
                                         (fn [t]
                                           (let [{:keys [target children] :as t} (chart/element c t)
                                                 s       (condition-symbol t)
                                                 actions (actions c children)]
                                             (cond
                                               (and target (= 1 (count target)))
                                               (cond-> {:target (str "#" (name (first target)))}
                                                 s (assoc :cond s)
                                                 (seq actions) (assoc :actions actions))
                                               (and (empty? target) event (seq actions))
                                               {:actions actions}
                                               :else nil)))
                                         ts))))
                              {}
                              transitions-by-event)]
    (when (seq event-map)
      {:on event-map})))

(declare statechart->xstate)

(defn state-map [c states]
  (reduce
   (fn [acc s]
     (let [{:keys [id] :as state} (chart/element c s)
           atomic?      (chart/atomic-state? c s)
            ;transitions  (transitions c state)
           substates    (chart/child-states c id)
           substate-map (reduce
                         (fn [acc s]
                           (merge acc (statechart->xstate c s)))
                         {}
                         substates)]
       (assoc acc (name id)
              (if atomic?
                (transitions c state)
                (merge
                 (transitions c state)
                 substate-map)))))
   {}
   states))

(defn state->xstate [c node]
  (if (chart/atomic-state? c node)
    (merge {:id (name (chart/element-id c node))}
           (transitions c node))
    (let [initial (chart/initial-element c node)]
      (merge
       (transitions c node)
       {:initial (name (:id initial))
        :id      (name (chart/element-id c node))
        :states  (reduce
                  (fn [acc s]
                    (assoc acc (name (chart/element-id c s))
                           (state->xstate c s)))
                  {}
                  (chart/child-states c node))}))))

(defn mock-guards [{::sc/keys [elements-by-id] :as c}]
  (let [cts (into []
                  (filter
                   #(and
                     (contains? % :cond)
                     (= :transition (:node-type %))))
                  (vals elements-by-id))]
    (reduce
     (fn [acc t]
       (if-let [s (condition-symbol t)]
         (assoc acc s 'CONSTANTLY_TRUE)
         acc))
     {}
     cts)))

(comment
  ;; subscription management is a statechart in my code base
  (state->xstate subscription-management subscription-management)
  (mock-guards subscription-management)

  (chart-to-tree subscription-management subscription-management))