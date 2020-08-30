(ns vrac.core
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [vrac.component :as vc]
            [vrac.util :refer [map-vals tag-id-class]])
  #?(:cljs (:require-macros vrac.core)))

;; A spec for the vrac components.
(s/def ::component
  (s/keys :req-un [::vc/id]
          :opt-un [::vc/name
                   ::vc/description
                   ::vc/props
                   ::vc/template]))

;; A spec for the defc macro's arguments.
(s/def ::defc-args
  (s/cat :var symbol?
         :props ::vc/props
         :options (s/? (s/keys :opt-un [::vc/id
                                        ::vc/name
                                        ::vc/description]))
         :template any?))

;; TODO: The process of the parsed-template should be easier if the tree is
;; enriched with all the information that one can grab.
;; Consider using a functional zipper for multi-directional navigation and node enrichment.
;; Consider using rules instead of functions when enriching the tree.
(defn template->ast [template]
  (s/conform ::vc/template template))

(defn ast->template [[kw val :as parsed-template]])
  ;; I am waiting for Clojure spec2 to support recursion to simply use s/unform.
  ;(s/unform ::vc/template parsed-template)

;; TODO: Instead of visiting nodes in each different function and collecting the children,
;; use a visitor or maybe a functional zipper.


;; A macro for writing the templates more like a function.
;; Note: in the end, the component is still just a map of data.
(defn- defc* [var props options template]
  (let [ns-str (name (ns-name *ns*))
        var-str (name var)
        default-id (keyword ns-str var-str)
        parsed-template (s/conform ::vc/template template)
        template-props (when-not (= parsed-template :clojure.spec.alpha/invalid))
                         ;(get-template-props parsed-template))
        missing-props (set/difference (set template-props)
                                      (set props))]
    (when (= parsed-template :clojure.spec.alpha/invalid)
      (throw (ex-info (str "Invalid template: " (s/explain-str ::vc/template template))
                      (s/explain-str ::vc/template template))))

    (when (seq missing-props)
      (throw (ex-info (str "Props missing from the declaration: " missing-props)
                      missing-props)))

    (merge {:id default-id}
           options
           `{:props '~props
             :template '~template

             ;; Those fields will go away in the long term, once they are used *only* at compilation phase.
             :template-props '~template-props
             :parsed-template '~parsed-template})))

;; A macro for writing the templates more like a function.
;; Note: in the end, the component is still just a map of data.
(defmacro defc [& args]
  (let [{:keys [var props options template]} (s/conform ::defc-args args)]
    `(def ~var ~(defc* var props options template))))

(defn with-components [env components]
  (assoc env
    :components (into {} (map (juxt :id identity)) components)))
