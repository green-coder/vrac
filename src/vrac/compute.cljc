(ns vrac.compute
  (:require [diffuse.core :as d]
            [diffuse.helper :as h]))

(def empty-subscriber-tree {})

(defn subscribe-on-path [subscriber-tree path subscriber]
  (if (seq path)
    (let [[path-element & path-rest] path]
      (update-in subscriber-tree [:children path-element]
                 subscribe-on-path path-rest subscriber))
    (update subscriber-tree :subscribers (fnil conj #{}) subscriber)))

(defn- update-coll-then-dissoc-empty [coll key f & args]
  (let [new-val (apply f (get coll key) args)]
    (if (seq new-val) ; new-val is assumed to be a collection
      (assoc coll key new-val)
      (dissoc coll key))))

(defn unsubscribe-from-path [subscriber-tree path subscriber]
  (if (seq path)
    (let [[path-element & path-rest] path]
      (update-coll-then-dissoc-empty subscriber-tree :children
        update-coll-then-dissoc-empty path-element
        unsubscribe-from-path path-rest subscriber))
    (update-coll-then-dissoc-empty subscriber-tree :subscribers
      disj subscriber)))

(defn diff->subscribers [state new-state diff subscriber-tree]
  (let [result-on-this-node (if (contains? subscriber-tree :subscribers)
                              (let [input {:state state
                                           :new-state new-state
                                           :diff diff}]
                                (mapv (fn [subscriber] [subscriber input]) (:subscribers subscriber-tree)))
                              [])
        tree-children (:children subscriber-tree)]
    (case (:type diff)
      :missing (into result-on-this-node
                     (mapcat (fn [[key child-tree]]
                               (diff->subscribers (get state key) nil h/missing child-tree)))
                     tree-children)
      :value (let [value (:value diff)
                   associative-value? (associative? value)]
               (into result-on-this-node
                     (mapcat (fn [[key child-tree]]
                               (let [state (get state key)
                                     [new-state diff] (if (and associative-value?
                                                               (contains? value key))
                                                        (let [value (get value key)]
                                                          [value (h/value value)])
                                                        [nil h/missing])]
                                 (diff->subscribers state new-state diff child-tree))))
                     tree-children))
      :set result-on-this-node
      :map (into result-on-this-node
                 (mapcat (fn [[key op]]
                           (when (contains? tree-children key)
                             (let [state (get state key)
                                   child-tree (get tree-children key)]
                               (case (first op)
                                 :assoc (let [value (second op)]
                                          (diff->subscribers state value (h/value value) child-tree))
                                 :update (diff->subscribers state (get new-state key) (second op) child-tree)
                                 :dissoc (diff->subscribers state nil h/missing child-tree))))))
                 (:key-op diff))
      :vector (->> (loop [index-ops (:index-op diff)
                          idx 0
                          results []]
                     (if (seq index-ops)
                       (let [[[op op-arg] & index-ops] index-ops]
                         (case op
                           :no-op (recur index-ops (+ idx op-arg) results)
                           :update (recur index-ops (+ idx (count op-arg))
                                          (into results
                                                (map-indexed (fn [index update-diff]
                                                               (let [index (+ idx index)]
                                                                 (when (contains? tree-children index)
                                                                   (diff->subscribers (get state index)
                                                                                      (get new-state index)
                                                                                      update-diff
                                                                                      (get tree-children index))))))
                                                op-arg))
                           :remove (recur index-ops (+ idx op-arg)
                                          (into results
                                                (map (fn [index]
                                                       (when (contains? tree-children index)
                                                         (diff->subscribers (get state index) nil h/missing (get tree-children index)))))
                                                (range idx (+ idx op-arg))))
                           :insert (recur index-ops idx results)))
                       results))
                   (into result-on-this-node cat)))))

(def empty-graph {})

#_{0 {;; Constant while in the graph.
      :node-id 0
      :compute-depth 0

      ;; The input keys are local ids.
      :inputs {:event {;:state nil
                       :format :diff}}
      :output {:format :diff}
      :compute-fn (fn [node]
                    (-> node :inputs-update :event :diff))

      ;; Changes when adding/removing dependent nodes in the graph.
      :subscriber-tree empty-subscriber-tree

      ;; Contains the updated input values (new-state and diff).
      :inputs-update {} ; {:event {:new-state new-state :diff diff}}

      ;; May change after each re-computation of the node.
      ;:state nil

      #__}}

(defmacro assoc-default [m k v]
  `(let [m# ~m
         k# ~k]
     (cond-> m# (not (contains? m# k#)) (assoc k# ~v))))

(defn add-graph-node
  "Returns a compute-graph with the node added.
   The node needs to have a unique node-id and a correct depth."
  [graph node]
  (let [node-id (:node-id node)
        node (-> node
                 (assoc :subscriber-tree empty-subscriber-tree
                        :inputs-update {})
                 ;(assoc-default :state nil)
                 (assoc-default :compute-depth (inc (apply max (map (fn [[input-key input]]
                                                                      (-> graph (get (:node-id input)) :compute-depth))
                                                                    (:inputs node))))))]
    (-> (reduce (fn [graph [input-key input]]
                  (cond-> graph
                    (contains? graph (:node-id input))
                    (update-in [(:node-id input) :subscriber-tree]
                               subscribe-on-path (:path input) [node-id input-key])))
                graph
                (:inputs node))
        (assoc node-id node))))

(defn remove-graph-node
  "Returns a compute-graph with the node removed.
   The user has to make sure that no other nodes depend on the node to remove."
  [graph node-id]
  (-> (reduce (fn [graph [input-key input]]
                (update-in graph [(:node-id input) :subscriber-tree]
                           unsubscribe-from-path (:path input) [node-id input-key]))
              graph
              (-> graph (get node-id) :inputs))
      (dissoc node-id)))

(def initial-graph
  (add-graph-node empty-graph
                  {:node-id 0
                   :compute-depth 0
                   :inputs {:event {}}
                   :output {:format :diff}
                   :compute-fn (fn [node]
                                 (-> node :inputs-update :event :diff))}))
(def empty-priority-queue
  (sorted-set-by (fn [left right]
                   (< (first left) (first right)))))

(defn- update-dependents-inputs
  "Update the dependent nodes' inputs.

   Example of inputs-updates:
   `[[[node-id input-key] {:state ..., :new-state ..., :diff ...}]]`"
  [graph inputs-updates]
  (reduce (fn
            ([graph] graph)
            ([graph [[node-id input-key] input-value]]
             (assoc-in graph [node-id :inputs-update input-key] input-value)))
          graph
          inputs-updates))

(defn- update-priority-queue
  "Add the nodes in the priority queue, if they are not already there."
  [priority-queue graph inputs-updates]
  (into priority-queue
        (comp (map (fn [[[node-id input-key] input-value]]
                     (when-let [priority (-> graph (get node-id) :compute-depth)]
                       [priority node-id])))
              (remove nil?))
        inputs-updates))

;(defn- format-input
;  "Transforms an input value into the format expected by the compute-fn."
;  [input input-state]
;  (let [input-state (cond-> input-state
;                      (= (:diff input-state) h/missing)
;                      (assoc :new-state (:missing-value input)))
;        format (:format input)]
;    (cond
;      (keyword? format) (format input-state)
;      (vector? format) ((apply juxt format) input-state)
;      (set? format) (select-keys format input-state)
;      (fn? format) (format input-state)
;      :else input-state)))

(defn propagate-diff
  "Returns a new graph with the effects of a diff propagated."
  [diff graph]
  (loop [priority-queue (conj empty-priority-queue [0 0])
         graph (assoc-in graph [0 :inputs-update :event] {:diff diff})]
    (if (empty? priority-queue)
      graph
      (let [; Pop the first node in the queue
            [_ node-id :as node-entry] (first priority-queue)
            priority-queue (disj priority-queue node-entry)

            ; Run the compute-fn on the node
            node (get graph node-id)
            state (:state node)
            output ((:compute-fn node) node)
            [new-state diff] (case (-> node :output (:format :diff))
                               :new-state [output (h/value output)]
                               :diff [(d/apply output state) output])

            ; Store the new input's state, and clear the input updates.
            node (assoc node
                   :inputs (reduce (fn [inputs [input-key inputs-state]]
                                     (update inputs input-key
                                             assoc :state (:new-state inputs-state)))
                                   (:inputs node)
                                   (:inputs-update node))
                   :inputs-update {})]

        (if (nil? diff)
          (recur priority-queue (assoc graph node-id node))
          (let [; Save this node's state
                node (assoc node :state new-state)

                ; Propagate the diff to subscribers
                inputs-updates (diff->subscribers state new-state diff (:subscriber-tree node))

                ; Set the values on a subscriber' inputs
                graph (update-dependents-inputs graph inputs-updates)

                ; Add the nodes to the priority queue, if they are not already there.
                priority-queue (update-priority-queue priority-queue graph inputs-updates)]
            (recur priority-queue (assoc graph node-id node))))))))
