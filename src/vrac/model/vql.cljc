(ns vrac.model.vql
  (:require [minimallist.core :as m]
            [minimallist.helper :as h]))

(def vql-model
  (h/let ['constant (h/alt [:nil (h/val nil)]
                           [:boolean (h/enum #{true false})]
                           [:number (h/fn number?)]
                           [:keyword (h/fn keyword?)]
                           [:string (h/fn string?)])
          ; the left part in a binding pair
          'binding (h/alt [:destruct/symbol (-> (h/fn simple-symbol?)
                                                (h/with-condition (h/fn (complement #{'&}))))]
                          [:destruct/vector (-> (h/cat [:destruct.vector/elements (h/* (h/not-inlined (h/ref 'binding)))]
                                                       [:destruct.vector/rest-syntax (h/? (h/cat (h/val '&)
                                                                                                 [:destruct.vector/rest (h/not-inlined (h/ref 'binding))]))])
                                                (h/in-vector))]
                          [:destruct/map (h/map-of (h/alt [:destruct.map/keyword (h/vector [:binding (h/ref 'binding)]
                                                                                           [:keyword (h/fn keyword?)])]
                                                          [:destruct.map/keys (h/vector [:keys-kw (-> (h/fn keyword?)
                                                                                                      (h/with-condition (h/fn (comp #{"keys"} name))))]
                                                                                        [:symbols (h/vector-of (h/fn symbol?))])]
                                                          [:destruct.map/or (h/vector (h/val :or)
                                                                                      [:defaults (h/map-of (h/vector [:symbol (h/fn simple-symbol?)]
                                                                                                                     [:value (h/ref 'node)]))])]
                                                          [:destruct.map/as (h/vector (h/val :as)
                                                                                      [:symbol (h/fn simple-symbol?)])]))])
          'bindings (-> (h/* (h/cat [:binding (h/not-inlined (h/ref 'binding))]
                                    [:value (h/not-inlined (h/ref 'node))])))
          'node (h/alt [:constant (h/ref 'constant)]
                       [:variable (h/fn symbol?)]
                       [:keyword-deref (-> (h/cat [:keyword (h/fn keyword?)]
                                                  [:data (h/not-inlined (h/ref 'node))]
                                                  [:default-value (h/? (h/not-inlined (h/ref 'node)))])
                                           (h/in-list))]
                       [:fn-call (-> (h/cat [:fn (-> (h/fn symbol?)
                                                     (h/with-condition (h/fn (complement #{'let 'for 'if 'when}))))]
                                            [:args (h/* (h/not-inlined (h/ref 'node)))])
                                     (h/in-list))]
                       [:vector (h/vector-of (h/ref 'node))]
                       [:hashmap (h/map-of (h/vector [:key (h/ref 'constant)]
                                                     [:value (h/ref 'node)]))]
                       [:let (h/list (h/val 'let)
                                     [:bindings (h/ref 'bindings)]
                                     [:body (h/ref 'node)])]
                       [:for (h/list (h/val 'for)
                                     [:bindings (h/ref 'bindings)]
                                     [:body (h/ref 'node)])]
                       [:if (h/list (h/val 'if)
                                    [:condition (h/ref 'node)]
                                    [:then (h/ref 'node)]
                                    [:else (h/ref 'node)])]
                       [:when (h/list (h/val 'when)
                                      [:condition (h/ref 'node)]
                                      [:then (h/ref 'node)])])]
    (h/ref 'node)))
