(ns scratch
  "FIXME: my new org.corfield.new/scratch project."
  (:require
   [camel-snake-kebab.core :as csk]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]
   [com.fulcrologic.statecharts.chart :as chart :refer [statechart]]
   [com.fulcrologic.statecharts.elements :refer [state transition]]))

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

(defn state->tree [c node]
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
                           (state->tree c s)))
                  {}
                  (chart/child-states c node))}))))

; -------------- Statechart to Mermaid --------------

(def mermaid (atom []))

(defn add-mermaid-node
  ([from target]
   (swap! mermaid conj (str from " --> " target)))
  ([from target & {:keys [event]}]
   (swap! mermaid conj (str from " --> " target
                            (when event (str " : " event))))))

(defn process-state-node [c node-id]
  (let [node (chart/element c node-id)
        {:keys [node-type initial? event target children]} node]
    (when (= node-type :state)
      (when initial?
        (add-mermaid-node "[*]" (name node-id)))
      (doseq [child children]
        (process-state-node c child)))

    (when (and (= node-type :state) (nil? children))
      (add-mermaid-node (name node-id) "[*]"))

    (when (= node-type :transition)
      (let [from (name (:parent node))
            target (name (first target))
            event (name event)]
        (add-mermaid-node from target {:event event})))))

(defn state->mermaid [c]
  (let [{:keys [children]} c]
    (reset! mermaid [])
    (swap! mermaid conj "stateDiagram-v2")
    (doseq [node-id children] (process-state-node c node-id))
    @mermaid))

(def biker
  (statechart {}
              (state {:id :still
                      :initial true
                      :initial? :still}
                     (transition {:event :push :target :moving}))
              (state {:id :moving}
                     (transition {:event :pull :target :still})
                     (transition {:event :push :target :crash}))
              (state {:id :crash})))

(comment
  ;; OUTPUT
  ;; ["stateDiagram-v2"
  ;;  "[*] --> still"
  ;;  "still --> moving : push"
  ;;  "moving --> still : pull"
  ;;  "moving --> crash : push"
  ;;  "crash --> [*]"]
  (state->mermaid biker)

  biker

  (state->tree biker biker)
  (chart/initial-element biker biker)
  (chart/element biker :transition32593)

  nil)

;; (defn exec
;;   "Invoke me with clojure -X scratch/exec"
;;   [opts]
;;   (println "exec with" opts))

;; (defn -main
;;   "Invoke me with clojure -M -m scratch"
;;   [& args]
;;   (println "-main with" args))
