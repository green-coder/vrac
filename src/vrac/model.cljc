(ns vrac.model
  (:require [clojure.walk :as w]
            [minimallist.core :as m]
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


;; The format which can be found in the fn, let, form, with,
;; and many other forms which feature binding & destructuring.
(def binding-destruct-model
  (h/let ['param-binding (h/alt [:binding-symbol (h/fn symbol?)]
                                [:destruct-vector (h/vector-of (h/ref 'param-binding))]
                                ;; todo: cover the general syntax, this one is simplified
                                [:destruct-map (h/map-of (h/ref 'param-binding)
                                                         (h/fn keyword?))])]
    (h/ref 'param-binding)))


(def template-params-model
  ; Variadic params are not supported at the moment.
  (h/vector-of binding-destruct-model))


(def template-body-model
  (h/let ['kw-deref (-> (h/cat [:keyword (h/fn keyword?)]
                               [:value (h/not-inlined (h/ref 'clj-value))]
                               [:default (h/? (h/not-inlined (h/ref 'clj-value)))])
                        h/in-list)

          'fn-call (-> (h/cat [:fn-symb (-> (h/fn symbol?)
                                            (h/with-condition (h/fn (complement #{'if 'when 'let 'for
                                                                                  'val 'attrs
                                                                                  'clojure.core/unquote-splicing}))))]
                              [:args (h/* (h/not-inlined (h/ref 'clj-value)))])
                       h/in-list)

          'if (h/list (h/val 'if)
                      [:condition (h/ref 'clj-value)]
                      [:then (h/ref 'clj-value)]
                      [:else (h/ref 'clj-value)])

          'when (h/list (h/val 'when)
                        [:condition (h/ref 'clj-value)]
                        [:then (h/ref 'clj-value)])

          'bindings (-> (h/* (h/cat [:symbol (h/fn simple-symbol?)]
                                    [:clj-value (h/not-inlined (h/ref 'clj-value))]))
                        h/in-vector)

          'let (h/list (h/val 'let)
                       [:bindings (h/ref 'bindings)]
                       [:body (h/ref 'clj-value)])

          'for (h/list (h/val 'for)
                       [:bindings (h/ref 'bindings)]
                       [:body (h/ref 'clj-value)])

          'val-wrap (h/list (h/val 'val)
                            [:clj-value (h/ref 'clj-value)])

          'clj-value (h/alt [:clj/nil (h/val nil)]
                            [:clj/boolean (h/fn boolean?)]
                            [:clj/number (h/fn number?)]
                            [:clj/string (h/fn string?)]
                            [:clj/keyword (h/fn keyword?)]
                            [:clj/symbol (h/fn symbol?)]
                            [:clj/vector (h/vector-of (h/ref 'clj-value))]
                            [:clj/set (h/set-of (h/ref 'clj-value))]
                            [:clj/hashmap (h/map-of (h/ref 'clj-value) (h/ref 'clj-value))]
                            [:clj/if (h/ref 'if)]
                            [:clj/when (h/ref 'when)]
                            [:clj/let (h/ref 'let)]
                            [:clj/for (h/ref 'for)]
                            [:clj/kw-deref (h/ref 'kw-deref)]
                            [:clj/val-wrap (h/ref 'val-wrap)]
                            [:clj/fn-call (h/ref 'fn-call)])

          ; Will be converted into text or nil, (or html for debug logging).
          'html/dom-leaf (h/alt [:clj/nil (h/val nil)]
                                [:clj/boolean (h/fn boolean?)]
                                [:clj/number (h/fn number?)]
                                [:clj/string (h/fn string?)]
                                [:clj/keyword (h/fn keyword?)]
                                [:clj/symbol (h/fn symbol?)]
                                ;[:clj/vector (h/vector-of (h/ref 'clj-value))]
                                [:clj/set (h/set-of (h/ref 'clj-value))]
                                [:clj/hashmap (h/map-of (h/ref 'clj-value) (h/ref 'clj-value))]
                                [:clj/kw-deref (h/ref 'kw-deref)]
                                [:clj/val-wrap (h/ref 'val-wrap)]
                                [:clj/fn-call (h/ref 'fn-call)])

          'html/if (h/list (h/val 'if)
                           [:condition (h/ref 'clj-value)]
                           [:then (h/ref 'html/something)]
                           [:else (h/ref 'html/something)])

          'html/when (h/list (h/val 'when)
                             [:condition (h/ref 'clj-value)]
                             [:then (h/ref 'html/something)])

          'html/let (h/list (h/val 'let)
                            [:bindings (h/ref 'bindings)]
                            [:body (h/ref 'html/something)])

          'html/for (h/list (h/val 'for)
                            [:bindings (h/ref 'bindings)]
                            [:body (h/ref 'html/something)])

          'unquote-splicing-wrap (h/list [:unquote-splicing-symb (h/val 'clojure.core/unquote-splicing)]
                                         ;; TODO: this should be a collection of html/something
                                         [:clj-value (h/ref 'clj-value)])

          'attrs-wrap (h/list (h/val 'attrs)
                              [:clj-value (h/ref 'clj-value)])

          'html/props (h/alt [:html.props/nil (h/val nil)]
                             [:html.props/hashmap (h/map-of (h/fn keyword?) (h/ref 'clj-value))]
                             [:html.props/attrs (h/ref 'attrs-wrap)])

          ; div, h1 ..
          'html/dom-element (-> (h/cat [:type (h/fn simple-keyword?)]
                                       [:props (h/? (h/not-inlined (h/ref 'html/props)))]
                                       [:children (h/* (h/not-inlined (h/ref 'html/something)))])
                                h/in-vector)

          ; my-comp, foo/bar ..
          'html/component (-> (h/cat [:component (h/fn qualified-keyword?)]
                                     [:props (h/? (h/not-inlined (h/ref 'html/props)))]
                                     [:children (h/* (h/not-inlined (h/ref 'html/something)))])
                              h/in-vector)

          ;; In a context assumed to be html, represents something.
          'html/something (h/alt [:html/dom-leaf (h/ref 'html/dom-leaf)]
                                 [:html/dom-element (h/ref 'html/dom-element)]
                                 [:html/component (h/ref 'html/component)]
                                 [:html/if (h/ref 'html/if)]
                                 [:html/when (h/ref 'html/when)]
                                 [:html/let (h/ref 'html/let)]
                                 [:html/for (h/ref 'html/for)]
                                 [:html/unquote-splicing-wrap (h/ref 'unquote-splicing-wrap)])]
    (h/ref 'html/something)))


; Similar to a defn signature:
; [name doc-string? attr-map? [params*] body]
; [name doc-string? attr-map? ([params*] body)+]
(def defc-args-model
  (h/let ['attr-map (-> (h/map)
                        (h/with-optional-entries [:id (h/fn qualified-keyword?)]
                                                 [:name (h/fn string?)]))
          ; Variadic params are not supported at the moment.
          'param-bindings (h/vector-of (h/fn any?))
          'body (h/fn any?)
          'params-body-cat (h/cat [:params (h/not-inlined (h/ref 'param-bindings))]
                                  [:body (h/not-inlined (h/ref 'body))])]
    (h/cat [:name (h/fn simple-symbol?)]
           [:doc-string (h/? (h/fn string?))]
           [:attr-map (h/? (h/ref 'attr-map))]
           [:definitions (h/alt [:single-arity (h/ref 'params-body-cat)]
                                [:multi-arities (h/+ (h/in-list (h/ref 'params-body-cat)))])])))


(defn- v-pair? [x]
  (and (vector? x)
       (= 2 (count x))))


(defn- simpler-ast [tree]
  (if (v-pair? tree)
    (let [[key val] tree]
      (case key
        ; Unwrap things - they were wrapped just for context clarification during parsing.
        :clj/val-wrap (:clj-value val)
        :html.props/attrs (:clj-value val)

        ; optional values h/?, [] / [x] -> nil / x
        :html/dom-element [key (update val :props first)]
        :html/component [key (update val :props first)]
        :clj/kw-deref [key (update val :default first)]
        tree))
    tree))

(defn template-params->ast [params]
  (->> params
       (m/describe template-params-model)
       (w/postwalk simpler-ast)))

(defn template-body->ast [body]
  (->> body
       (m/describe template-body-model)
       (w/postwalk simpler-ast)))


;; TODO: The process of the parsed-template should be easier if the tree is
;; enriched with all the information that one can grab.
;; Consider using a functional zipper for multi-directional navigation and node enrichment.
;; Consider using rules instead of functions when enriching the tree.


;; TODO: Instead of visiting nodes in each different function and collecting the children,
;; use a visitor or maybe a functional zipper.
;; or maybe interface minimallist with specter.

(comment

  ;; html/if
  (template-body->ast '[:div (if condition [:h1 "big title"] [:h5 "small title"])])
  (template-body->ast '[:div (if condition "big title" "small title")])

  ;; if
  (template-body->ast '[:div (val (if condition "big title" "small title"))])

  ;; html/when
  (template-body->ast '[:div (when condition [:h1 "big title"])])
  (template-body->ast '[:div (when condition "big title")])

  ;; when
  (template-body->ast '[:div (val (when condition "big title"))])

  ;; html/let
  (template-body->ast '(let [a 1, b 2] [:div a b]))

  ;; let
  (template-body->ast '(val (let [a 1, b 2] {a b})))

  ;; html/for
  (template-body->ast '[:ul (for [a [1 2 3], b [:a :b :c]] [:li a b])])

  ;; for
  (template-body->ast '(val (for [a [1 2 3], b [:a :b :c]] {a b})))


  (template-body->ast [:div "hello, " nil "world!"])
  (template-body->ast [:div nil "hello, " true "world!"])
  (template-body->ast [:div {} {} "hello"])
  (template-body->ast [:div {:id :theme-name
                             :style {:color "pink"}} "Green"])
  (template-body->ast [:div {:id :theme-name
                             :class [:color "button"]
                             :style {:color "pink"}} "Green"])
  (template-body->ast '[:div "debug info: " {:id user-id, :user user-name}])
  (template-body->ast '[:div.debug-info nil {:id user-id, :user user-name}])
  (template-body->ast '[:div.debug-info (val {:id user-id, :user user-name})])
  (template-body->ast '[:div.debug-info (val [:a :b :c :d])])
  (template-body->ast '[:div.debug-info (attrs my-attributes)])

  ;; Including a component
  (template-body->ast '[:my-ns/my-component "hello"])

  ;; Deprecated construct - invalid
  ;(template-body->ast '[my-component "hello"])
  ;(template-body->ast '[(if my-cond ::my-component ::other-comp) "hello"])
  ;(template-body->ast '[(if my-cond ::my-component my-alias/other-comp) "hello"])

  ;; Deprecated construct - invalid
  ;; A component can have any identifier except unqualified keywords which are reserved for html elements.
  ;(template-body->ast '[true "This is not a pipe"])
  ;(template-body->ast '[false "This is a pipe"])
  ;(template-body->ast '[42 "This could be anything"])
  ;(template-body->ast '[foobar "hello"])
  ;(template-body->ast '[[:experiment 7] "hello"])

  ;; Arguments passing
  (template-body->ast '[::my-component true false nil "foo"])           ;; any value can be passed, including nil
  (template-body->ast '[::my-component (val [:a :b]) (val [c d])])      ;; vector literals should be marked as values
  (template-body->ast '[::my-component (str "peace and " my-love-var)]) ;; any s-expression can be passed
  (template-body->ast '[::my-component :a :b ~@my-kw-sequence :z])      ;; ~@ slices an expression in the arg list

  ;; Attributes applied on components
  (template-body->ast '[::my-component.foo {:class "bar"} arg1 arg2])    ;; my-component only has 2 args
  (template-body->ast '[::my-component (attrs my-attributes) arg1 arg2]) ;; my-component only has 2 args

  ;; Function calls
  (template-body->ast '[::my-component (str "peace and " my-love-var)])
  (template-body->ast '(str "peace and " my-love-var))

  ;; Keyword deref
  (template-body->ast '(:a global))
  (template-body->ast '(:a global 5))

  #__)
