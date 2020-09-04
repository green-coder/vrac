(ns vrac.compute
  (:require [diffuse.core :as d]
            [diffuse.helper :as h]))

(def empty-subscriber-tree {})

(defn subscribe-on-path [subscriber-tree path subscriber missing-value]
  (if (seq path)
    (let [[path-element & path-rest] path]
      (update-in subscriber-tree [:children path-element]
                 subscribe-on-path path-rest subscriber missing-value))
    (update subscriber-tree :subscribers assoc subscriber missing-value)))

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
      dissoc subscriber)))

(defn propagate-diff [subscriber-tree diff]
  (let [result-on-this-node (if (contains? subscriber-tree :subscribers)
                              [[(:subscribers subscriber-tree) diff]]
                              [])
        tree-children (:children subscriber-tree)]
    (case (:type diff)
      :missing (into result-on-this-node
                     (mapcat (fn [child-tree]
                               (propagate-diff child-tree h/missing)))
                     (vals tree-children))
      :value (let [value (:value diff)
                   associative-value? (associative? value)]
               (into result-on-this-node
                     (mapcat (fn [[key child-tree]]
                               (->> (if (and associative-value? (contains? value key))
                                      (h/value (get value key))
                                      h/missing)
                                    (propagate-diff child-tree))))
                     tree-children))
      :set result-on-this-node
      :map (into result-on-this-node
                 (mapcat (fn [[key op]]
                           (when (contains? tree-children key)
                             (->> (case (first op)
                                    :assoc (h/value (second op))
                                    :update (second op)
                                    :dissoc h/missing)
                                  (propagate-diff (get tree-children key))))))
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
                                                                   (propagate-diff (get tree-children index) update-diff)))))
                                                op-arg))
                           :remove (recur index-ops (+ idx op-arg)
                                          (into results
                                                (map (fn [index]
                                                       (when (contains? tree-children index)
                                                         (propagate-diff (get tree-children index) h/missing))))
                                                (range idx (+ idx op-arg))))
                           :insert (recur index-ops idx results)))
                       results))
                   (into result-on-this-node cat)))))
