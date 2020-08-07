(ns vrac.component
  (:require [vrac.template :as vt]
            [clojure.spec.alpha :as s]))

(s/def ::id qualified-keyword?)

(s/def ::name string?)

(s/def ::description string?)

;; Only needed for documenting meta data:
;; ^:eql vs. ^:val, ^{:as 'local-symb}, ^{:default 3}, etc...
(s/def ::props (s/coll-of symbol? :kind vector?))

(s/def ::template ::vt/node)
