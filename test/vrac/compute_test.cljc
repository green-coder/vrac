(ns vrac.compute-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are] :include-macros true])
            [diffuse.core :as d]
            [diffuse.helper :as h]
            [vrac.compute :refer [empty-subscriber-tree
                                  subscribe-on-path
                                  unsubscribe-from-path
                                  propagate-diff]]))

(deftest subscribe-on-path-test
  (is (= (-> empty-subscriber-tree
             (subscribe-on-path [:user :first-name] 1 :not-found))
         {:children {:user {:children {:first-name {:subscribers {1 :not-found}}}}}}))

  (is (= (-> empty-subscriber-tree
             (subscribe-on-path [:user :first-name] 1 :not-found)
             (subscribe-on-path [:user :last-name] 2 nil))
         {:children {:user {:children {:first-name {:subscribers {1 :not-found}}
                                       :last-name  {:subscribers {2 nil}}}}}}))

  (is (= (-> empty-subscriber-tree
             (subscribe-on-path [:user :first-name] 1 :not-found)
             (subscribe-on-path [:user :last-name] 2 nil)
             (subscribe-on-path [:user] 3 :nobody))
         {:children {:user {:subscribers {3 :nobody},
                            :children {:first-name {:subscribers {1 :not-found}}
                                       :last-name  {:subscribers {2 nil}}}}}})))


(deftest unsubscribe-from-path-test
  (is (= (-> empty-subscriber-tree
             (subscribe-on-path [:user :first-name] 1 :not-found)
             (subscribe-on-path [:user :last-name] 2 nil)
             (subscribe-on-path [:user] 3 :nobody)
             (unsubscribe-from-path [:user :last-name] 2))
         (-> empty-subscriber-tree
             (subscribe-on-path [:user :first-name] 1 :not-found)
             (subscribe-on-path [:user] 3 :nobody))))

  (is (= (-> empty-subscriber-tree
             (subscribe-on-path [:user :first-name] 1 :not-found)
             (subscribe-on-path [:user :last-name] 2 nil)
             (subscribe-on-path [:user] 3 :nobody)
             (unsubscribe-from-path [:user] 3))
         (-> empty-subscriber-tree
             (subscribe-on-path [:user :first-name] 1 :not-found)
             (subscribe-on-path [:user :last-name] 2 nil))))

  (is (= (-> empty-subscriber-tree
             (subscribe-on-path [:user :first-name] 1 :not-found)
             (subscribe-on-path [:user :last-name] 2 nil)
             (subscribe-on-path [:user] 3 :nobody)
             (unsubscribe-from-path [:user :first-name] 1)
             (unsubscribe-from-path [:user :last-name] 2)
             (unsubscribe-from-path [:user] 3))
         empty-subscriber-tree)))


(deftest propagate-diff-test
  (let [subscriber-tree (-> empty-subscriber-tree
                          (subscribe-on-path [:user :first-name] 1 :not-found)
                          (subscribe-on-path [:user :last-name] 2 nil)
                          (subscribe-on-path [:user] 3 :nobody)
                          (subscribe-on-path [] 4 :no-root))]

    ;; missing
    (is (= (propagate-diff subscriber-tree
                           h/missing)
           [[{4 :no-root} h/missing]
            [{3 :nobody} h/missing]
            [{1 :not-found} h/missing]
            [{2 nil} h/missing]]))

    ;; value
    (is (= (propagate-diff subscriber-tree
                           (h/value {:user {:first-name "Alice"
                                            :last-name "in wonderland"}}))
           [[{4 :no-root} (h/value {:user {:first-name "Alice"
                                           :last-name "in wonderland"}})]
            [{3 :nobody} (h/value {:first-name "Alice"
                                   :last-name "in wonderland"})]
            [{1 :not-found} (h/value "Alice")]
            [{2 nil} (h/value "in wonderland")]]))

    ;; value
    (is (= (propagate-diff subscriber-tree
                           (h/value {:user "Bob"}))
           [[{4 :no-root} (h/value {:user "Bob"})]
            [{3 :nobody} (h/value "Bob")]
            [{1 :not-found} h/missing]
            [{2 nil} h/missing]]))

    ;; set-conj
    (is (= (propagate-diff (-> subscriber-tree
                               (subscribe-on-path [:logged-users] 5 #{}))
                           (h/map-update :logged-users (h/set-conj "Bob")))
           [[{4 :no-root} (h/map-update :logged-users (h/set-conj "Bob"))]
            [{5 #{}} (h/set-conj "Bob")]]))

    ;; map-dissoc
    (is (= (propagate-diff subscriber-tree
                           (h/map-dissoc :user))
           [[{4 :no-root} (h/map-dissoc :user)]
            [{3 :nobody} h/missing]
            [{1 :not-found} h/missing]
            [{2 nil} h/missing]]))

    ;; map-update map-dissoc
    (is (= (propagate-diff subscriber-tree
                           (h/map-update :user (h/map-dissoc :first-name)))
           [[{4 :no-root} (h/map-update :user (h/map-dissoc :first-name))]
            [{3 :nobody} (h/map-dissoc :first-name)]
            [{1 :not-found} h/missing]]))

    ;; map-update map-assoc
    (is (= (propagate-diff subscriber-tree
                           (h/map-update :user (h/map-assoc :first-name "Coco")))
           [[{4 :no-root} (h/map-update :user (h/map-assoc :first-name "Coco"))]
            [{3 :nobody} (h/map-assoc :first-name "Coco")]
            [{1 :not-found} (h/value "Coco")]]))

    ;; map-update map-assoc
    (is (= (propagate-diff subscriber-tree
                           (h/map-update :user (h/map-assoc :last-name "the cat")))
           [[{4 :no-root} (h/map-update :user (h/map-assoc :last-name "the cat"))]
            [{3 :nobody} (h/map-assoc :last-name "the cat")]
            [{2 nil} (h/value "the cat")]]))

    ;; vec-assoc (replaces an element)
    (is (= (propagate-diff (-> subscriber-tree
                               (subscribe-on-path [:items] 6 :nothing)
                               (subscribe-on-path [:items 3] 7 nil))
                           (h/map-update :items (h/vec-assoc 3 {:name "Chocolatine"})))
           [[{4 :no-root} (h/map-update :items (h/vec-assoc 3 {:name "Chocolatine"}))]
            [{6 :nothing} (h/vec-assoc 3 {:name "Chocolatine"})]
            [{7 nil} h/missing]]))

    ;; vec-update (updates an element)
    (is (= (propagate-diff (-> subscriber-tree
                               (subscribe-on-path [:items] 6 :nothing)
                               (subscribe-on-path [:items 3] 7 nil))
                           (h/map-update :items (h/vec-update 3 (h/map-assoc :name "Chocolatine"))))
           [[{4 :no-root} (h/map-update :items (h/vec-update 3 (h/map-assoc :name "Chocolatine")))]
            [{6 :nothing} (h/vec-update 3 (h/map-assoc :name "Chocolatine"))]
            [{7 nil} (h/map-assoc :name "Chocolatine")]]))

    #__))
