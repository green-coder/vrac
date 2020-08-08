(ns vrac.model
  (:require [minimallist.helper :as h]))

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


(def model-definitions
  ['primitive (h/alt [:boolean (h/fn boolean?)]
                     [:number (h/fn number?)]
                     [:string (h/fn string?)]
                     [:keyword (h/fn keyword?)]
                     [:symbol (h/fn symbol?)])

   'kw-hashmap (h/map-of (h/fn keyword?) (h/ref 'valuable))

   'kw-deref (h/list [:keyword (h/fn keyword?)]
                     [:value (h/alt [:nil (h/val nil)]
                                    [:symbol (h/fn simple-symbol?)]
                                    [:kw-hashmap (h/ref 'kw-hashmap)]
                                    [:kw-deref (h/ref 'kw-deref)])])

   'valuable (h/alt [:nil (h/val nil)]
                    [:primitive (h/ref 'primitive)]
                    [:vector (h/vector-of (h/ref 'valuable))]
                    [:set (h/set-of (h/ref 'valuable))]
                    [:hashmap (h/map-of (h/ref 'valuable) (h/ref 'valuable))]
                    [:kw-deref (h/ref 'kw-deref)])

   'printable (h/alt [:nil (h/val nil)]
                     [:primitive (h/ref 'primitive)])

   'props (h/ref 'kw-hashmap)
   'html (-> (h/cat [:type (h/fn simple-keyword?)]
                    [:props (h/? (h/ref 'props))]
                    [:children (h/* (h/not-inlined (h/ref 'node)))])
             h/in-vector)
   'component (-> (h/cat [:type (h/fn qualified-keyword?)]
                         [:props (h/? (h/ref 'props))]
                         [:children (h/* (h/not-inlined (h/ref 'node)))])
                  h/in-vector)

   'condition (h/ref 'valuable) ; to be improved later
   'if (h/list [:name (h/val 'if)]
               [:condition (h/ref 'condition)]
               [:then (h/ref 'node)]
               [:else (h/ref 'node)])
   'when (h/list [:name (h/val 'when)]
                 [:condition (h/ref 'condition)]
                 [:then (h/ref 'node)])

   'bindings (-> (h/* (h/cat [:symbol (h/fn simple-symbol?)]
                             [:valuable (h/not-inlined (h/ref 'valuable))]))
                 h/in-vector)
   'let (h/list [:name (h/val 'let)]
                [:bindings (h/ref 'bindings)]
                [:body (h/ref 'node)])
   'for (h/list [:name (h/val 'for)]
                [:bindings (h/ref 'bindings)]
                [:body (h/ref 'node)])

   'directive (h/alt [:if (h/ref 'if)]
                     [:when (h/ref 'when)]
                     [:let (h/ref 'let)]
                     [:for (h/ref 'for)])

   'node (h/alt [:leaf (h/ref 'printable)]
                [:html (h/ref 'html)]
                [:component (h/ref 'component)]
                [:directive (h/ref 'directive)])])

(def template (h/let model-definitions (h/ref 'node)))
