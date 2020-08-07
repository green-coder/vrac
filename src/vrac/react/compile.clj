(ns vrac.react.compile
  (:refer-clojure :exclude [compile])
  (:require [vrac.core :as v]))

;; Compilation

(def default-env
  ;{:create-element 'js/React.createElement}
  {:create-element 'react/createElement})

; to do later
(defn compile-props [props]
  props)

(defn compile-template
  "Outputs a React render function's body that return React's virtual DOM."
  ([parsed-template]
   (compile-template parsed-template default-env))

  ([parsed-template {:keys [create-element] :as env}]
   (letfn [(compile [template]
             (cond
               (and (vector? template)
                    (= (count template) 2))
               (let [[kw val] template]
                 (case kw
                   (:nil :boolean :number :string :keyword :symbol) val
                   :map (into {} (map (fn [[k v]]
                                        [(compile k) (compile v)])) val)
                   :get-kw (let [{:keys [keyword valuable]} val
                                 valuable (compile valuable)]
                             `(~keyword ~valuable))
                   :if (let [{:keys [cond then else]} val
                             [cond then else] (map compile [cond then else])]
                         `(if ~cond ~then ~else))
                   :when (let [{:keys [cond then]} val]
                           [cond then] (map compile [cond then])
                           `(when ~cond ~then))
                   :let (let [{:keys [bindings body]} val
                              bindings (into [] (mapcat (fn [{:keys [symbol valuable]}]
                                                          [symbol (compile valuable)])
                                                        bindings))
                              body (compile body)]
                          `(~'let ~bindings ~body))
                   :for (let [{:keys [bindings body]} val
                              bindings (into [] (mapcat (fn [{:keys [symbol valuable]}]
                                                          [symbol (compile valuable)])
                                                        bindings))
                              body (compile body)]
                          `(~'for ~bindings ~body))
                   :comp (let [{:keys [keyword props children]} val
                               props (compile-props props)
                               children (map compile children)]
                           `(~create-element
                              ~(name keyword)
                              ~props
                              ~@children))
                   (compile val)))

               :else nil))]
     (compile parsed-template))))


(defmacro render-fn [template]
  (let [parsed-template (v/template->ast template)
        props (v/get-template-props parsed-template)
        body (compile-template parsed-template)]
    `(~'fn [{:keys [~@props]}] ~body)))

(comment
  (def template '[:div
                  [:p "name: " (:name user)]
                  (let [age (:age user)]
                    [:p "age: " age])
                  [:p "friends: "
                   [:ul
                    (for [buddy (:friends user)]
                      [:li
                       [:p "name: " (:name buddy)]
                       [:p "age: " (:age buddy)]])]]])

  (compile-template (v/template->ast template))

  (-> '(let [age (:age user)]
         [:p "age: " age])
      v/template->ast
      compile-template))
