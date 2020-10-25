(ns vrac.model.vql
  (:require [minimallist.core :as m]
            [minimallist.helper :as h]))

(def vql-model
  (h/let ['constant (h/alt [:number (h/fn number?)]
                           [:keyword (h/fn keyword?)]
                           [:string (h/fn string?)]
                           [:boolean (h/enum #{true false})])
          ; the left part in a binding pair
          'binding (h/alt [:destruct/symbol (h/fn simple-symbol?)]
                          [:destruct/vector (h/vector-of (h/ref 'binding))]
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
                                                     (h/with-condition (h/fn (complement #{'let 'for 'if 'when true false}))))]
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

(m/describe vql-model
            '[(:session/title session)
              (:session/description session)
              (for [speakers (:session/speakers session)]
                [(:speaker/name speakers)
                 (:speaker/bio speakers)])])

(m/describe vql-model
            '(let [{:session/keys [title description speakers]} session]
               [title
                session
                (for [{:speaker/keys [name bio]} speakers]
                  {:name name
                   :bio bio})]))


(comment
  ; EQL
  '{session [:session/title
             :session/description
             {:session/speakers [:speaker/name
                                 :speaker/bio]}]}

  ; VQL
  '[(:session/title session)
    (:session/description session)
    (for [speakers (:session/speakers session)]
      [(:speaker/name speakers)
       (:speaker/bio speakers)])]

  ;; VQL - same semantic as above
  '(let [{:session/keys [title description speakers]} session]
     [title
      session
      (for [{:speaker/keys [name bio]} speakers]
        [name
         bio])])

  ; Returns values similarly to Clojure
  ["session title"
   "session description"
   [["name1"
     "bio1"]
    ["name2"
     "bio2"]]]

  ; VQL - computed data
  '[(- (:admin.money/spent global)
       (:admin.money/earned global))
    (count (:admin/users global))]

  ; VQL - let with computed data
  '(let [{:admin/keys [users]
          :admin.money/keys [earning spending]} global
         benefit (- earning spending)
         nb-users (count users)]
     [benefit
      nb-users
      (/ benefit nb-users)]))
