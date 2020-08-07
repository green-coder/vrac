(ns vrac.template
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

; Vrac's template (i.e. the format used by the user)

; - [:ns/vrac-comp ... children]
; - [:html-tag ... children]
; - (dsl-node ... children)
;   for now, only `if`, `when`, `for` and `let` are supported.
; - variable name (i.e. a symbol) referring to either a value or an eql context.
; - immediate value

; (idea for later) notation for children as a list:
; '[comp . children]

; TODO: fix the generators w.r.t. (s/and vector?) and (s/and list?)

(s/def ::get-kw
  (s/and list?
         (s/cat :keyword keyword?
                :valuable ::valuable)))

(s/def ::valuable
  (s/or :nil nil?
        :boolean boolean?
        :number number?
        :string string?
        :keyword keyword?
        :symbol symbol?
        :map (s/map-of ::valuable ::valuable :conform-keys true)
        :get-kw ::get-kw))

(s/def ::node
  (s/or :comp ::comp
        :dsl ::dsl
        :valuable ::valuable))

(s/def ::props
  (s/map-of keyword? ::valuable))

(s/def ::comp
  (s/and vector?
         (s/cat :keyword keyword?
                :props (s/? ::props)
                :children (s/* ::node))))

(s/def ::bindings
  (s/and vector?
         (s/* (s/cat :symbol simple-symbol?
                     :valuable ::valuable))))

(s/def ::if
  (s/cat :directive #{'if}
         :cond ::valuable
         :then ::node
         :else ::node))

(s/def ::when
  (s/cat :directive #{'when}
         :cond ::valuable
         :then ::node))

(s/def ::let
  (s/cat :directive #{'let}
         :bindings ::bindings
         :body ::node))

(s/def ::for
  (s/cat :directive #{'for}
         :bindings ::bindings
         :body ::node))

(s/def ::dsl
  (s/and list?
         (s/alt :if ::if
                :when ::when
                :let ::let
                :for ::for)))
