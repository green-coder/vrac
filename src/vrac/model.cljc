(ns vrac.model
  (:require [minimallist.core :as m]
            [minimallist.helper :as h]))

;; Note 1:
;; It won't be possible to generate templates so easily, because there
;; are properties which can only be enforced on a not-so-local scale.
;; For instance, symbols have to be defined in let blocks.

;; Note 2:
;; The model cannot be precise enough to validate some properties which
;; are not-so-local as well.
;; For instance, that symbols are defined in let blocks.

;; What this model brings is a way to put a name on each part of the template
;; for templates which are valid. More validation has to be made manually.

;; Notes imported from the template.cljc file (which is going to be deleted):
; - [:ns/vrac-comp ... children]
; - [:html-tag ... children]
; - (dsl-node ... children)
;   for now, only `if`, `when`, `for` and `let` are supported.
; - variable name (i.e. a symbol) referring to either a value or an eql context.
; - immediate value

; (idea for later) notation for children as a list:
; '[comp . children]


;(def model-definitions
;  ['primitive (h/alt [:boolean (h/fn boolean?)]
;                     [:number (h/fn number?)]
;                     [:string (h/fn string?)]
;                     [:keyword (h/fn keyword?)]
;                     [:symbol (h/fn symbol?)])
;
;   'kw-hashmap (h/map-of (h/fn keyword?) (h/ref 'valuable))
;
;   'kw-deref (h/list [:keyword (h/fn keyword?)]
;                     [:value (h/alt [:nil (h/val nil)]
;                                    [:symbol (h/fn simple-symbol?)]
;                                    [:kw-hashmap (h/ref 'kw-hashmap)]
;                                    [:kw-deref (h/ref 'kw-deref)])])
;
;   'valuable (h/alt [:nil (h/val nil)]
;                    [:primitive (h/ref 'primitive)]
;                    [:vector (h/vector-of (h/ref 'valuable))]
;                    [:set (h/set-of (h/ref 'valuable))]
;                    [:hashmap (h/map-of (h/ref 'valuable) (h/ref 'valuable))]
;                    [:kw-deref (h/ref 'kw-deref)])
;
;   'printable (h/alt [:nil (h/val nil)]
;                     [:primitive (h/ref 'primitive)])
;
;   'props (h/ref 'kw-hashmap)
;   'html (-> (h/cat [:type (h/fn simple-keyword?)]
;                    [:props (h/? (h/ref 'props))]
;                    [:children (h/* (h/not-inlined (h/ref 'node)))])
;             h/in-vector)
;   'component (-> (h/cat [:type (h/fn qualified-keyword?)]
;                         [:props (h/? (h/ref 'props))]
;                         [:children (h/* (h/not-inlined (h/ref 'node)))])
;                  h/in-vector)
;
;   'condition (h/ref 'valuable) ; to be improved later
;   'if (h/list [:name (h/val 'if)]
;               [:condition (h/ref 'condition)]
;               [:then (h/ref 'node)]
;               [:else (h/ref 'node)])
;   'when (h/list [:name (h/val 'when)]
;                 [:condition (h/ref 'condition)]
;                 [:then (h/ref 'node)])
;
;   'bindings (-> (h/* (h/cat [:symbol (h/fn simple-symbol?)]
;                             [:valuable (h/not-inlined (h/ref 'valuable))]))
;                 h/in-vector)
;   'let (h/list [:name (h/val 'let)]
;                [:bindings (h/ref 'bindings)]
;                [:body (h/ref 'node)])
;   'for (h/list [:name (h/val 'for)]
;                [:bindings (h/ref 'bindings)]
;                [:body (h/ref 'node)])
;
;   'directive (h/alt [:if (h/ref 'if)]
;                     [:when (h/ref 'when)]
;                     [:let (h/ref 'let)]
;                     [:for (h/ref 'for)])
;
;   'node (h/alt [:leaf (h/ref 'printable)]
;                [:html (h/ref 'html)]
;                [:component (h/ref 'component)]
;                [:directive (h/ref 'directive)])])
;
;(def template (h/let model-definitions (h/ref 'node)))


;; ---------------------------------------------------------------
;; September 18th 2020's new model.

(def template-model
  (h/let [#_#_'fn-call (-> (h/cat [:fn 'clj-value]
                                  [:args (h/* (h/not-inlined (h/ref 'clj-value)))])
                           (h/in-list))

          'clj-value (h/alt [:nil (h/val nil)]
                            [:boolean (h/fn boolean?)]
                            [:number (h/fn number?)]
                            [:string (h/fn string?)]
                            [:keyword (h/fn keyword?)]
                            [:symbol (h/fn symbol?)]
                            [:vector (h/vector-of (h/ref 'clj-value))]
                            [:set (h/set-of (h/ref 'clj-value))]
                            [:hashmap (h/map-of (h/ref 'clj-value) (h/ref 'clj-value))])
                            ;[:val-wrap (h/ref 'val-wrap)]
                            ;[:fn-call (h/ref 'html/fn-call)])

          'html/clj-value (h/alt [:nil (h/val nil)]
                                 [:boolean (h/fn boolean?)]
                                 [:number (h/fn number?)]
                                 [:string (h/fn string?)]
                                 [:keyword (h/fn keyword?)]
                                 [:symbol (h/fn symbol?)]
                                 ;[:vector (h/vector-of (h/ref 'clj-value))]
                                 [:set (h/set-of (h/ref 'clj-value))]
                                 [:hashmap (h/map-of (h/ref 'clj-value) (h/ref 'clj-value))]
                                 [:val-wrap (h/list [:val-symb (h/val 'val)]
                                                    [:clj-value (h/ref 'clj-value)])])
                                 ;[:unquote-splicing-wrap (h/list [:unquote-splicing-symb (h/val 'clojure.core/unquote-splicing)]
                                 ;                                [:clj-value (h/ref 'clj-value)])])
                                 ;[:fn-call (h/ref 'html/fn-call)])

          'html/props (h/alt [:nil (h/val nil)]
                             [:hashmap (h/map-of (h/fn keyword?) (h/ref 'clj-value))]
                             [:attrs (h/list [:attrs-symb (h/val 'attrs)]
                                             [:clj-value (h/ref 'clj-value)])])

          'html/dom-element (-> (h/cat ; div, h1 ..
                                       [:type (h/fn simple-keyword?)]

                                       ; optional props
                                       [:props (h/? (h/ref 'html/props))]

                                       [:children (h/* (h/not-inlined (h/ref 'html/something)))])
                                h/in-vector)

          'html/component (-> (h/cat ; my-comp, foo/bar ..
                                     [:component (-> (h/not-inlined (h/ref 'clj-value))
                                                     (h/with-condition (h/fn (complement simple-keyword?))))]

                                     ; optional props
                                     [:props (h/? (h/ref 'html/props))]

                                     [:children (h/* (h/not-inlined (h/ref 'html/something)))])
                              h/in-vector)

          ;; In a context assumed to be html, represents something.
          'html/something (h/alt [:html/clj-value (h/ref 'html/clj-value)]
                                 ; will be converted into text or nil, (or html for debug logging).
                                 [:html/dom-element (h/ref 'html/dom-element)]
                                 [:html/component (h/ref 'html/component)])]
                       ;[:directive (h/ref 'directive)])]
    (h/ref 'html/something)))

(comment
  (m/describe template-model '[:my-ns/my-component "hello"])
  (m/describe template-model '[my-component "hello"])
  ;(m/describe template-model '[(if my-cond ::my-component ::other-comp) "hello"])
  ;(m/describe template-model '[(if my-cond ::my-component my-alias/other-comp) "hello"])

  ;; A component can have any identifier except unqualified keywords which are reserved for html elements.
  (m/describe template-model '[true "This is not a pipe"])
  (m/describe template-model '[false "This is a pipe"])
  (m/describe template-model '[42 "This could be anything"])
  (m/describe template-model '[foobar "hello"])
  (m/describe template-model '[[:experiment 7] "hello"])

  ;; Arguments passing
  (m/describe template-model '[::my-component true false nil "foo"])           ;; any value can be passed, including nil
  (m/describe template-model '[::my-component (val [:a :b]) (val [c d])])      ;; vector literals should be marked as values
  ;(m/describe template-model '[::my-component (str "peace and " my-love-var)]) ;; any s-expression can be passed
  ;(m/describe template-model '[::my-component :a :b ~@my-kw-sequence :z])      ;; ~@ slices an expression in the arg list

  ;; Attributes applied on components
  (m/describe template-model '[::my-component.foo {:class "bar"} arg1 arg2])    ;; my-component only has 2 args
  (m/describe template-model '[::my-component (attrs my-attributes) arg1 arg2])) ;; my-component only has 2 args


(comment
  (m/describe template-model [:div "hello, " nil "world!"])
  (m/describe template-model [:div nil "hello, " nil "world!"])
  (m/describe template-model [:div {} {} "hello"])
  (m/describe template-model [:div {:id :theme-name
                                    :style {:color "pink"}} "Green"])
  (m/describe template-model [:div {:id :theme-name
                                    :class [:color "button"]
                                    :style {:color "pink"}} "Green"])
  (m/describe template-model '[:div "debug info: " {:id user-id, :user user-name}])
  (m/describe template-model '[:div.debug-info nil {:id user-id, :user user-name}])
  (m/describe template-model '[:div.debug-info (val {:id user-id, :user user-name})])
  (m/describe template-model '[:div.debug-info (val [:a :b :c :d])])
  (m/describe template-model '[:div.debug-info (attrs my-attributes)]))
