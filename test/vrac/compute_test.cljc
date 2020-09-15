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


;; This data will be generated from the template, with a unique node-id and a correct compute-depth.
(def compute-node-1
  {;; Constant while in the graph.
   :node-id 1
   ;:compute-depth 1
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

   #__})

(def render-node-1
  {:node-id 2
   :render-depth 1
   :inputs {:full-name {:node-id 1
                        :path []
                        :missing-value nil}}
   :output {:format :new-state}
   ; will be changed later
   :compute-fn (fn [node]
                 [:div (-> node :inputs-update :full-name :new-state)])})

(def full-name-graph
  (-> initial-graph
      (add-graph-node compute-node-1)
      (add-graph-node render-node-1)))

(comment

  (->> full-name-graph
       (propagate-diff (h/value {:user {:first-name "Bob", :last-name "Marley"}}))
       (propagate-diff (h/map-update :user (h/map-assoc :first-name "Coco"))))

  #__)


;; -------------------------------------------------------------------------------------
;; For loop

; db
{:items [{:name "Apple" :price 8}
         {:name "Banana" :price 10}
         {:name "Strawberry" :price 16}]}

; output of the mapv node
[{:name "Apple" :price 9}
 {:name "Banana" :price 11}
 {:name "Strawberry" :price 17}]

; Make a change in the db
(h/map-update :items (h/vec-insert 2 {:name "Coconut" :price 12}))

; Update on the derived array
[{:name "Apple" :price 9}
 {:name "Banana" :price 11}
 {:name "Coconut" :price 13}
 {:name "Strawberry" :price 17}]


(defn inc-node [node-id item-index]
  {:node-id node-id
   :inputs [{:node-id 0
             :path [:items item-index :price]}]
   :compute-fn (fn [node]
                 (h/value (-> node :inputs-update 0 :new-state inc)))})

;; TODO:
;; - move the nodes into a :nodes sub hashmap.
;; - add a :next-id 0 entry beside it.
;; - the root node gets the 0 id when added.

; '[:ul (for [name (:name-list global)]
;         [:li name])]

(let [for-body-node (fn [item-index]
                      {:node-id (+ 2 item-index)
                       :inputs [{:node-id 0
                                 :path [:name-list item-index]}]
                       :compute-fn (fn [name]
                                     [:li name])})
      for-node {:node-id 1
                :inputs [{:node-id 0
                          :path [:name-list]}]
                ;:compute-depth 10
                :compute-fn (fn [name-list]
                              (:diff name-list))
                :transform-graph-fn (fn [graph node-id])}])

;; We don't want to get notified of all the inputs each time one of them changes.
;; For example, we don't want to have a list of 100 persons, when 1 person changes.
;; What we could do instead is receiving diff of what has changed among the inputs.
;; It would mean receiving only 1 input in the shape of a diff, which is created as
;; the combination (via d/comp) of the change provided by all the diffs on the inputs
;; which has changed.

;; Basic use case with a `for`.
'[:ul (for [name (:name-list global)]
        [:li name])]

{:name-list ["Alice" "Bob" "Coco"]}
'[:ul
  [:li "Alice"]
  [:li "Bob"]
  [:li "Coco"]]


;; Complete use case with a `for` and a `if`.
'[:h3 "Full name list:"
  [:ul (for [{:keys [first-name last-name] :as user} (:user-list global)]
         [:li (if (nil? last-name)
                first-name
                (str first-name " " last-name))])]]
