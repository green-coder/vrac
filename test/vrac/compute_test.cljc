(ns vrac.compute-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are] :include-macros true])
            [diffuse.core :as d]
            [diffuse.helper :as h]
            [vrac.compute :as c :refer [empty-subscriber-tree
                                        subscribe-on-path
                                        unsubscribe-from-path
                                        diff->subscribers
                                        initial-graph
                                        add-graph-node
                                        remove-graph-node
                                        empty-priority-queue
                                        propagate-diff]]))

(deftest subscribe-on-path-test
  (is (= (-> empty-subscriber-tree
             (subscribe-on-path [:user :first-name] 1))
         {:children {:user {:children {:first-name {:subscribers #{1}}}}}}))

  (is (= (-> empty-subscriber-tree
             (subscribe-on-path [:user :first-name] 1)
             (subscribe-on-path [:user :last-name] 2))
         {:children {:user {:children {:first-name {:subscribers #{1}}
                                       :last-name  {:subscribers #{2}}}}}}))

  (is (= (-> empty-subscriber-tree
             (subscribe-on-path [:user :first-name] 1)
             (subscribe-on-path [:user :last-name] 2)
             (subscribe-on-path [:user] 3))
         {:children {:user {:subscribers #{3},
                            :children {:first-name {:subscribers #{1}}
                                       :last-name  {:subscribers #{2}}}}}})))


(deftest unsubscribe-from-path-test
  (is (= (-> empty-subscriber-tree
             (subscribe-on-path [:user :first-name] 1)
             (subscribe-on-path [:user :last-name] 2)
             (subscribe-on-path [:user] 3)
             (unsubscribe-from-path [:user :last-name] 2))
         (-> empty-subscriber-tree
             (subscribe-on-path [:user :first-name] 1)
             (subscribe-on-path [:user] 3))))

  (is (= (-> empty-subscriber-tree
             (subscribe-on-path [:user :first-name] 1)
             (subscribe-on-path [:user :last-name] 2)
             (subscribe-on-path [:user] 3)
             (unsubscribe-from-path [:user] 3))
         (-> empty-subscriber-tree
             (subscribe-on-path [:user :first-name] 1)
             (subscribe-on-path [:user :last-name] 2))))

  (is (= (-> empty-subscriber-tree
             (subscribe-on-path [:user :first-name] 1)
             (subscribe-on-path [:user :last-name] 2)
             (subscribe-on-path [:user] 3)
             (unsubscribe-from-path [:user :first-name] 1)
             (unsubscribe-from-path [:user :last-name] 2)
             (unsubscribe-from-path [:user] 3))
         empty-subscriber-tree)))

(defn state-diff->subscribers [state diff subscribe-tree]
  (let [new-state (d/apply diff state)]
    (diff->subscribers state new-state diff subscribe-tree)))

(deftest diff->subscribers-test
  (let [state {:user {:first-name "Alice"
                      :last-name "in wonderland"}}
        subscriber-tree (-> empty-subscriber-tree
                          (subscribe-on-path [:user :first-name] 1)
                          (subscribe-on-path [:user :last-name] 2)
                          (subscribe-on-path [:user] 3)
                          (subscribe-on-path [] 4))]

    ;; missing
    (is (= (state-diff->subscribers state h/missing subscriber-tree)
           [[4 {:state state
                :new-state nil
                :diff h/missing}]
            [3 {:state (-> state :user)
                :new-state nil
                :diff h/missing}]
            [1 {:state (-> state :user :first-name)
                :new-state nil
                :diff h/missing}]
            [2 {:state (-> state :user :last-name)
                :new-state nil
                :diff h/missing}]]))

    ;; value
    (let [new-state {:user {:first-name "Bob"
                            :last-name "Marley"}}]
      (is (= (state-diff->subscribers state (h/value new-state) subscriber-tree)
             [[4 {:state state
                  :new-state new-state
                  :diff (h/value new-state)}]
              [3 {:state (-> state :user)
                  :new-state (-> new-state :user)
                  :diff (h/value (-> new-state :user))}]
              [1 {:state (-> state :user :first-name)
                  :new-state (-> new-state :user :first-name)
                  :diff (h/value (-> new-state :user :first-name))}]
              [2 {:state (-> state :user :last-name)
                  :new-state (-> new-state :user :last-name)
                  :diff (h/value (-> new-state :user :last-name))}]])))

    ;; value, structure changed
    (let [new-state {:user "Bob"}]
      (is (= (state-diff->subscribers state (h/value new-state) subscriber-tree)
             [[4 {:state state
                  :new-state new-state
                  :diff (h/value new-state)}]
              [3 {:state (-> state :user)
                  :new-state (-> new-state :user)
                  :diff (h/value (-> new-state :user))}]
              [1 {:state (-> state :user :first-name)
                  :new-state (-> new-state :user :first-name)
                  :diff h/missing}]
              [2 {:state (-> state :user :last-name)
                  :new-state (-> new-state :user :last-name)
                  :diff h/missing}]])))

    ;; set-conj
    (let [state {:logged-users nil}
          diff (h/map-update :logged-users (h/set-conj "Alice"))
          new-state (d/apply diff state)
          subscriber-tree (-> empty-subscriber-tree
                              (subscribe-on-path [] 1)
                              (subscribe-on-path [:logged-users] 2))]
      (is (= (state-diff->subscribers state diff subscriber-tree)
             [[1 {:state state
                  :new-state (update new-state :logged-users conj "Alice")
                  :diff diff}]
              [2 {:state (-> state :logged-users)
                  :new-state #{"Alice"}
                  :diff (h/set-conj "Alice")}]])))

    ;; map-dissoc
    (is (= (state-diff->subscribers state (h/map-dissoc :user) subscriber-tree)
           [[4 {:state state
                :new-state (dissoc state :user)
                :diff (h/map-dissoc :user)}]
            [3 {:state (-> state :user)
                :new-state nil
                :diff h/missing}]
            [1 {:state (-> state :user :first-name)
                :new-state nil
                :diff h/missing}]
            [2 {:state (-> state :user :last-name)
                :new-state nil
                :diff h/missing}]]))

    ;; map-update map-dissoc
    (is (= (state-diff->subscribers state
                                    (h/map-update :user (h/map-dissoc :first-name))
                                    subscriber-tree)
           [[4 {:state state
                :new-state (update state :user dissoc :first-name)
                :diff (h/map-update :user (h/map-dissoc :first-name))}]
            [3 {:state (-> state :user)
                :new-state (-> state :user (dissoc :first-name))
                :diff (h/map-dissoc :first-name)}]
            [1 {:state (-> state :user :first-name)
                :new-state nil
                :diff h/missing}]]))

    ;; same as above, but on a nil state
    (let [state nil]
      (is (= (state-diff->subscribers state
                                      (h/map-update :user (h/map-dissoc :first-name))
                                      subscriber-tree)
             [[4 {:state state
                  :new-state (update state :user dissoc :first-name)
                  :diff (h/map-update :user (h/map-dissoc :first-name))}]
              [3 {:state (-> state :user)
                  :new-state (-> state :user (dissoc :first-name))
                  :diff (h/map-dissoc :first-name)}]
              [1 {:state (-> state :user :first-name)
                  :new-state nil
                  :diff h/missing}]])))

    ;; map-update map-assoc
    (is (= (state-diff->subscribers state
                                    (h/map-update :user (h/map-assoc :first-name "Coco"))
                                    subscriber-tree)
           [[4 {:state state
                :new-state (update state :user assoc :first-name "Coco")
                :diff (h/map-update :user (h/map-assoc :first-name "Coco"))}]
            [3 {:state (-> state :user)
                :new-state (-> state :user (assoc :first-name "Coco"))
                :diff (h/map-assoc :first-name "Coco")}]
            [1 {:state (-> state :user :first-name)
                :new-state "Coco"
                :diff (h/value "Coco")}]]))

    ;; vec-assoc (replaces an element)
    (let [state {:items [nil nil nil {:name "Pain au chocolat"} nil]}
          new-state {:items [nil nil nil {:name "Chocolatine"} nil]}
          diff (h/map-update :items (h/vec-assoc 3 {:name "Chocolatine"}))
          subscriber-tree (-> empty-subscriber-tree
                              (subscribe-on-path [] 1)
                              (subscribe-on-path [:items] 2)
                              (subscribe-on-path [:items 3] 3))]
      (is (= (state-diff->subscribers state diff subscriber-tree)
             [[1 {:state state
                  :new-state new-state
                  :diff diff}]
              [2 {:state (-> state :items)
                  :new-state (-> new-state :items)
                  :diff (h/vec-assoc 3 {:name "Chocolatine"})}]
              [3 {:state (-> state :items (get 3))
                  :new-state nil
                  :diff h/missing}]])))

    ;; vec-update (updates an element)
    (let [state {:items [nil nil nil {:name "Pain au chocolat"} nil]}
          new-state {:items [nil nil nil {:name "Chocolatine"} nil]}
          diff (h/map-update :items (h/vec-update 3 (h/map-assoc :name "Chocolatine")))
          subscriber-tree (-> empty-subscriber-tree
                              (subscribe-on-path [] 1)
                              (subscribe-on-path [:items] 2)
                              (subscribe-on-path [:items 3] 3)
                              (subscribe-on-path [:items 3 :name] 4))]
      (is (= (state-diff->subscribers state diff subscriber-tree)
             [[1 {:state state
                  :new-state new-state
                  :diff diff}]
              [2 {:state (-> state :items)
                  :new-state (-> new-state :items)
                  :diff (h/vec-update 3 (h/map-assoc :name "Chocolatine"))}]
              [3 {:state (-> state :items (get 3))
                  :new-state (-> new-state :items (get 3))
                  :diff (h/map-assoc :name "Chocolatine")}]
              [4 {:state (-> state :items (get 3) :name)
                  :new-state "Chocolatine"
                  :diff (h/value "Chocolatine")}]])))

    #__))

(deftest priority-queue-test
  (is (= (seq (into empty-priority-queue [[3 :a] [1 :c] [2 :b]]))
         (list [1 :c] [2 :b] [3 :a])))

  (is (= (first (into empty-priority-queue [[3 :a] [1 :c] [2 :b]]))
         [1 :c]))

  (is (= (disj (into empty-priority-queue [[3 :a] [1 :c] [2 :b]]) [3 :a])
         (into empty-priority-queue [[2 :b] [1 :c]]))))


(deftest propagate-diff-test
  (let [compute-node-1
        {;:compute-depth 1
         :inputs {:first-name {:node-id 0 ; 0 is always the client db
                               :path [:user :first-name]
                               ; value to be used for new-state when diff is h/missing.
                               :missing-value nil}
                  :last-name {:node-id 0
                              :path [:user :last-name]
                              :missing-value nil}}
         :output {; Output format can be either :new-state or :diff, :diff by default.
                  :format :new-state}
         :compute-fn (fn [node]
                       (str (or (-> node :inputs-update :first-name :new-state)
                                (-> node :inputs :first-name :state))
                            " "
                            (or (-> node :inputs-update :last-name :new-state)
                                (-> node :inputs :last-name :state))))
         #__}

        render-node-1
        {:render-depth 1
         :inputs {:full-name {:node-id 1
                              :path []
                              :missing-value nil}}
         :output {:format :new-state}
         ; will be changed later
         :compute-fn (fn [node]
                       [:div (-> node :inputs-update :full-name :new-state)])}

        full-name-graph
        (-> initial-graph
            (add-graph-node compute-node-1)
            (add-graph-node render-node-1))]

    (is (= (-> full-name-graph
               :nodes (get 2) :state)
           nil))

    (is (= (-> (->> full-name-graph
                 (propagate-diff (h/value {:user {:first-name "Bob", :last-name "Marley"}})))
               :nodes (get 2) :state)
           [:div "Bob Marley"]))

    (is (= (-> (->> full-name-graph
                 (propagate-diff (h/value {:user {:first-name "Bob", :last-name "Marley"}}))
                 (propagate-diff (h/map-update :user (h/map-assoc :first-name "Coco"))))
               :nodes (get 2) :state)
           [:div "Coco Marley"]))))



;; -------------------------------------------------------------------------------------
;; (mapv (fn [x] (update :price inc)) ...)

; db
{:items [{:name "Apple" :price 8}
         {:name "Banana" :price 10}
         {:name "Strawberry" :price 16}]}

; output of the mapv node
[{:name "Apple" :price 9}
 {:name "Banana" :price 11}
 {:name "Strawberry" :price 17}]

; Make a change in the db
(h/map-update :items (h/vec-insert 2 [{:name "Coconut" :price 12}]))

; Change as output of the mapv node
(h/vec-insert 2 [{:name "Coconut" :price 13}])

; Update on the derived array
[{:name "Apple" :price 9}
 {:name "Banana" :price 11}
 {:name "Coconut" :price 13}
 {:name "Strawberry" :price 17}]


(defn inc-node [input-node-id input-value-path]
  {:inputs {:val {:node-id input-node-id
                  :path input-value-path}}
   :compute-fn (fn [node]
                 (h/value (-> node :inputs-update :val :new-state inc)))})

; '[:ul (for [name (:name-list global)]
;         [:li name])]

;; TODO: clear up the d/update function in diffuse
#_[:index-op (-> (h/vector-of (h/alt [:no-op (h/vector (h/val :no-op)
                                                       (h/ref 'size))]
                                     [:update (h/vector (h/val :update)
                                                        (h/in-vector (h/+ (h/ref 'diff))))]
                                     [:remove (h/vector (h/val :remove)
                                                        (h/ref 'size))]
                                     [:insert (h/vector (h/val :insert)
                                                        (h/in-vector (h/+ (h/ref 'value))))])))]
; Problem:
; If each node maintains a state, then there is the cost of the insertion into large arrays.
; It would be better if we don't have to maintain a state in each node by default.
; Side note: diff->subscribe-tree does not really need to distribute state and new-state,
; the subscribing node can find those states if it needs to.
; Side node 2: When we dynamically add nodes, they need to know the state, not just "the diff of the moment".
; I should develop the rendering and the template further, to better identify what/when/where are my needs w.r.t. state vs. diff.

(defn mapv-node [input-node-id input-coll-path]
  {:inputs {:coll {:node-id input-node-id
                   :path input-coll-path}}
   :transform-graph-fn (fn [graph node-id]
                         ;; TODO: add nodes in the graph when the diff contains additions
                         (let [node (-> graph :nodes (get node-id))
                               coll-update (-> node :inputs-update :coll)]
                           (if (nil? coll-update)
                             graph
                             (loop [[operation & operations] (-> coll-update :diff :index-op)
                                    index 0]
                               (case (first operation)
                                 :no-op (recur operations (+ index (second operation)))
                                 :update (recur operations (+ index (count (second operation))))
                                 :remove (recur operations (+ index (second operation)))
                                 :insert (recur operations (+ index (count (second operation)))))))))
   :compute-fn (fn [node]
                 ; TODO: Convert the diff on the inc items into a diff of a collection of them.
                 (-> :diff))})

(mapv-node 0 [:name-list])
