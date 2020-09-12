(ns vrac.compute-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are] :include-macros true])
            [diffuse.core :as d]
            [diffuse.helper :as h]
            [vrac.compute :as c :refer [empty-subscriber-tree
                                        subscribe-on-path
                                        unsubscribe-from-path
                                        diff->subscribers
                                        empty-graph
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


;; This data will be generated from the template, with a unique node-id and a correct compute-depth.
(def compute-node-1
  {;; Constant while in the graph.
   :node-id 1
   ;:compute-depth 1
   :inputs [{:node-id 0 ; 0 is always the client db
             :path [:user :first-name]
             :missing-value nil ; value to be used for new-state when diff is h/missing.
             ; Format describes how the data arrives in the function, and which data.
             ; It can have the following shapes:
             ; - :new-state for a single value, can be either :new-state or :diff.
             ; - [:state :new-state :diff] for a vector of values in the specified order.
             ; - #{:state :new-state :diff} for a hash-map of values.
             ; - a fn
             :format :new-state}
            {:node-id 0
             :path [:user :last-name]
             :missing-value nil
             :format :new-state}]
   :output {; Output format can be either :new-state or :diff, :diff by default.
            :format :new-state
            ; Should we inject the current state?
            :state-in-input? true}
   :compute-fn (fn [state first-name last-name]
                 (str first-name " " last-name))

   ;; Changes when adding/removing dependent nodes in the graph.
   :subscriber-tree empty-subscriber-tree

   ;; Changes when at least of of the inputs changes.
   :inputs-state [{:state nil
                   :new-state nil
                   :diff nil}]

   ;; Changes after each re-computation of the node.
   :state nil})

;; (if (< (:a global) (:b global))
;;   (:c global)
;;   (:d global))
;; Note: This example shows the general pattern.
;;       On a real case, (< (:a global) (:b global)) could be in a separate node.
(def branch-node-1
  {:type :branch
   :node-id 5
   :compute-depth 1
   :inputs [{:node-id 0
             :path [:a]
             :format :new-state}
            {:node-id 0
             :path [:b]
             :format :new-state}]
   :branches {:then {:node-id 0
                     :path [:c]
                     ;; nodes which need to be added to the graph when this branch is selected.
                     :nodes-fn (constantly [])}
              :else {:node-id 0
                     :path [:d]
                     :nodes-fn (constantly [])}}
   :branch-selection-fn (fn [a b]
                          (if (< a b)
                            ; (:c global)
                            :then
                            ; (:d global)
                            :else))})

(def render-node-1
  {:node-id 2
   :render-depth 1
   :inputs [{:node-id 1
             :path []
             :format :new-state
             :missing-value nil}]
   :output {:format :new-state}
   ; will be changed later
   :compute-fn (fn [full-name]
                 full-name)})

(def full-name-graph
  (-> empty-graph
      (add-graph-node compute-node-1)
      (add-graph-node render-node-1)))

(comment

  (->> full-name-graph
       (propagate-diff (h/value {:user {:first-name "Bob", :last-name "Marley"}}))
       (propagate-diff (h/map-update :user (h/map-assoc :first-name "Coco"))))

  #__)

