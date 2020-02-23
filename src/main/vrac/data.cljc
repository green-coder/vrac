(ns vrac.data
  (:refer-clojure :exclude [ident?])
  (:require [clojure.set :as set]))

(comment
  (defn matching-primary-keyword-finder
    "Returns a function that use the namespace of a keyword to find
     a matching primary keyword, supposing (by convention) that
     they both use the same namespace."
    [primary-kws]
    (let [ns->primary-kw (into {} (map (juxt namespace identity)) primary-kws)]
      (fn [kw]
        (ns->primary-kw (namespace kw))))))

(defn- get-primary-keywords
  "Returns a list of primary key's keywords found in the keys of the map m."
  [primary-kws m]
  (into [] (keep (comp primary-kws first)) m))

(defn ident
  "Returns an ident."
  [table id]
  ^:ident [table id])

(defn normalizer [primary-kws]
  "Takes a set of primary key keywords and returns a function that
   returns a list of elements of the shape [table id entity],
   where entity has each of its references to another entity replaced by
   its ident in the shape ^:ident [table id]."
  (fn normalize [entity]
    (cond
      (map? entity)
      (let [kws (get-primary-keywords primary-kws entity)
            kw-count (count kws)
            _ (when (> kw-count 1)
                (throw (ex-info (str "Entity has multiple primary keys: " kws)
                                entity)))
            [table-kw id-in-table] (if (zero? kw-count)
                                     [nil nil]
                                     [(first kws) (get entity (first kws))])
            [elements cut-entity] (reduce (fn [[res cut-entity] [k v]]
                                            (let [child-elements (normalize v)
                                                  last-element (peek child-elements)
                                                  [table id cut-v] last-element]
                                              (if (nil? table)
                                                [(into res (pop child-elements))
                                                 (assoc cut-entity k cut-v)]
                                                [(into res child-elements)
                                                 (assoc cut-entity k (ident table id))])))
                                          [[] {}]
                                          entity)]
        (conj elements [table-kw id-in-table cut-entity]))

      (coll? entity)
      (let [[elements cut-entities] (reduce (fn [[res cut-entities] v]
                                              (let [child-elements (normalize v)
                                                    last-element (peek child-elements)
                                                    [table id cut-v] last-element]
                                                (if (nil? table)
                                                  [(into res (pop child-elements))
                                                   (conj cut-entities cut-v)]
                                                  [(into res child-elements)
                                                   (conj cut-entities (ident table id))])))
                                            [[] (empty entity)]
                                            entity)]
        (conj elements [nil nil cut-entities]))

      :else [[nil nil entity]])))

;; OBSOLETE
(defn db-assoc
  "Assocs data coming from a normalizer to a db."
  [db normalized-elements]
  (reduce (fn [db [table id entity]]
            (assoc-in db [table id] entity))
          db
          normalized-elements))

;; OBSOLETE
(defn db-merge
  "Merges data coming from a normalizer to a db."
  [db normalized-elements]
  (reduce (fn [db [table id entity]]
            (update-in db [table id] merge entity))
          db
          normalized-elements))

;; ---------------------------------------------------------
;; 5 small utility functions that users are welcome to read,
;; understand, and inline in their code where they see fit.

(comment
  (defn db-assoc
    "Returns a db with the entity replacing previous data."
    ([db ident entity]
     (assoc-in db ident entity)))

  (defn db-dissoc
    "Returns a db with an entity removed."
    [db [table id :as ident]]
    (update db table dissoc id))

  (defn db-update
    "Returns a db with an updated entity."
    ([db ident f & args]
     (apply update-in db ident f args)))

  (defn db-merge
    "Returns a db with the entity merged on top of previous data."
    ([db ident entity]
     (update-in db ident merge entity)))

  (defn db-entity
    "Returns the entity referred by the ident in the db."
    [db ident]
    (get-in db ident)))

;; ---------------------------------------------------------

(defn ident?
  "Returns true if val is an ident, false otherwise."
  [val]
  (:ident (meta val)))

(defn deref-ident
  "Returns the pointed value in the db if val is an ident,
   returns val otherwise."
  [db val]
  (cond->> val
    (ident? val) (get-in db)))

;; Beware! Does not prevent infinite loops.
;;
;; Denormalization does not work in the general case, as the normalised db
;; represents a directed graph of data with potentially some cycles which
;; cannot be represented as a tree of data.
;;
;; This function is just here as an example, users are encouraged to work
;; directly on the normalized db, or to make their own function to
;; denormalize it according to their needs.
(defn denormalize-entity [db entity]
  (clojure.walk/prewalk (partial deref-ident db) entity))

(defn resolve-query
  "Resolves a simple EQL query on a normalized db.
   The query param is expected to be a vector."
  [db val query]
  (when-some [val (deref-ident db val)]
    (into {}
          (map (fn [query-elm]
                 (cond
                   (keyword? query-elm) [query-elm (query-elm val)]
                   (map? query-elm) (let [[k v] (first query-elm)
                                          val (k val)]
                                      [k (if (and (not (ident? val))
                                                  (coll? val))
                                           (into (empty val)
                                                 (map (fn [val-elm]
                                                        (resolve-query db val-elm v)))
                                                 val)
                                           (resolve-query db val v))]))))
          query)))

;; ---------------------------------------------------------

(defn apply-diff
  "Applies the change specified in the diff to data.

   When the `:kind` of diff is:
   - a map, diff has the following format:
     ```
     {:assoc {key0 val0, ...}
      :update {key0 diff0, ...}
      :dissoc [key0 ...]}
     ```
     No overlap is expected between the keys in :assoc, :update and :dissoc.
   - a set, diff has the following format:
     ```
     {:conj #{val0 ...}
      :disj #{val0 ...}}
     ```
     No overlap is expected between the vals in :conj and :disj.
   - a vector or a sequence, diff has the following format:
     ```
     {:assoc [[index0 val0] ...]
      :update [[index0 diff0] ...]
      :remsert [[false index0 n-elements0] ; remove
                [true index1 [val0 ...]]   ; insert
                ...]
     ```
     No overlap is expected between the indices in :assoc, :update and :remsert.
     The indices are referring to the original vector and are strictly increasing inside each operation.
     - `:assoc` replaces elements in the vector.
     - `:update` applies the diff to the elements.
     - `:remsert` either removes or inserts elements from/to the vector.
       For the same index, a removal should precede an insertion.
       Insertions are not expected to happen _inside_ removed parts.
   - a value, diff has the following format:
     ```
     {:value val}
     ```
     val replaces the data.

   When diff is nil, the data is returned."
  [data diff]
  (if (nil? diff)
    data
    (case (:kind diff)
      :map (as-> data xxx
                 (into xxx (:assoc diff))
                 (into xxx
                       (map (fn [[k v]]
                              [k (apply-diff (get data k) v)]))
                       (:update diff))
                 (apply dissoc xxx (:dissoc diff)))
      :set (as-> data xxx
                 (set/union xxx (:conj diff))
                 (set/difference xxx (:disj diff)))
      :vector (as-> data xxx
                    (if-let [diff-assoc (seq (:assoc diff))]
                      (apply assoc xxx (into [] cat diff-assoc))
                      xxx)
                    (if-let [diff-update (seq (:update diff))]
                      (apply assoc xxx (into []
                                             (mapcat (fn [[k v]]
                                                       [k (apply-diff (get data k) v)]))
                                             diff-update))
                      xxx)
                    (loop [index 0
                           remserts (:remsert diff)
                           result []]
                      (let [[insert? start-index n-elements] (first remserts)]
                        (cond
                          (nil? remserts)
                          (into result (subvec xxx index))

                          (< index start-index)
                          (recur start-index
                                remserts
                                (into result (subvec xxx index start-index)))

                          insert?
                          (recur index
                                (next remserts)
                                (into result n-elements))

                          :else
                          (recur (+ index n-elements)
                            (next remserts)
                            result)))))
      :value (:value diff))))

(def empty-diff
  "The empty diff represents a change with no effect."
  nil)

(defn value-diff
  "Returns a diff which represent a replacement by a given value."
  [val]
  {:kind :value
   :value val})

(defn merge-diff
  "Returns the diff which represents the merge operation.
   Useful on maps."
  [& maps]
  {:kind :map
   :assoc (apply merge maps)})

(defn assoc-diff
  "Returns the diff which represents the assoc operation.
  Useful on maps and vectors."
  [data & {:as kvs}]
  (if (map? data)
    {:kind :map
     :assoc kvs}
    {:kind :vector
     :assoc (into [] (sort-by first kvs))}))

(defn dissoc-diff
  "Returns the diff which represents the dissoc operation.
   Useful on maps."
  [& keys]
  {:kind :map
   :dissoc keys})

(defn conj-diff
  "Returns the diff which represents the conj operation.
   Useful on sets and vectors."
  [data & vals]
  (if (set? data)
    {:kind :set
     :conj (set vals)}
    {:kind :vector
     :remsert [[true (count data) (into [] data)]]}))

(defn disj-diff
  "Returns the diff which represents the disj operation.
   Useful on sets and vectors."
  [data & vals]
  (if (set? data)
    {:kind :set
     :disj (set vals)}
    {:kind :vector
     :remsert [[false (- (count data) (count vals)) (count vals)]]}))

(defn remove-diff
  "Returns the diff which represents the removal of a part in a vector."
  [index nb-elements]
  {:kind :vector
   :remsert [[false index nb-elements]]})

(defn insert-diff
  "Returns the diff which represents the insertion of elements in a vector."
  [index vals]
  {:kind :vector
   :remsert [[true index vals]]})

(defn update-diff
  "Returns a diff representing the composition of an update around the provided diff."
  [db key f-diff & params]
  (let [val (apply f-diff (get db key) params)]
    (if (map? db)
      {:kind :map
       :update {key val}}
      {:kind :vector
       :update [[key val]]})))

(defn update-in-diff
  "Returns a diff representing the composition of an update-in around the provided diff."
  [db path f-diff & params]
  (if (seq path)
    (let [key (first path)]
      (update-diff db key
                   #(apply update-in-diff % (next path) f-diff params)))
    (apply f-diff db params)))

(defn assoc-in-diff
  "Returns a diff representing the effect of an assoc-in operation."
  [db path val]
  (if (seq path)
    (update-in-diff db (butlast path) assoc-diff (last path) val)
    empty-diff))

(defn- normalized-elements->tree
  "Aggregate normalized elements into a tree structure."
  [normalized-elements]
  (reduce (fn [db [table id entity]]
            (assoc-in db [table id] entity))
          {}
          normalized-elements))

(defn db-assoc-diff
  "Returns a diff representing the effect of inserting `[table id entity]` elements in a normalized db."
  [normalized-elements]
  {:kind :map
   :update (into {}
                 (map (fn [[table id->entity]]
                        [table {:kind :map
                                :assoc id->entity}]))
                 (normalized-elements->tree normalized-elements))})

(defn db-merge-diff
  "Returns a diff representing the effect of updating `[table id entity]` elements in a normalized db."
  [normalized-elements]
  {:kind :map
   :update (into {}
                 (map (fn [[table id->entity]]
                        [table {:kind :map
                                :update (into {}
                                              (map (fn [[id entity]]
                                                     [id {:kind :map
                                                          :assoc entity}]))
                                              id->entity)}]))
                 (normalized-elements->tree normalized-elements))})

