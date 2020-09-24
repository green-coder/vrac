(ns vrac.core
  (:require [clojure.set :as set]
            [minimallist.core :as m]
            [vrac.model :as vm]
            [vrac.util :refer [map-vals tag-id-class]])
  #?(:cljs (:require-macros vrac.core)))


;; A macro for writing the templates more like a function.
;; Note: in the end, the component is still just a map of data.
(defmacro defc [& params]
  (let [desc (m/describe vm/defc-args-model params)]
    (when (= desc :invalid)
      (throw (ex-info (str "Invalid template: " #_(m/explain-str vm/defc-args-model params))
                      {:params params})))

    `(def ~(:name desc)
       {:id ~(or (get-in desc [:attr-map 0 :id])
                 (keyword (name (ns-name *ns*))
                          (name (:name desc))))
        :name ~(or (get-in desc [:attr-map 0 :name])
                   (get desc :name))
        :description ~(get-in desc [:doc-string 0])
        :templates '~(let [[arity-type defs] (:definitions desc)]
                       (into {}
                             (map (fn [{:keys [params body]}]
                                    [(count params) {:params params
                                                     :body body
                                                     :params-ast (vm/template-params->ast params)
                                                     :body-ast (vm/template-body->ast body)}]))
                             (case arity-type
                              :single-arity [defs]
                              :multi-arities defs)))})))

(comment
  (macroexpand-1 '(defc todo-item
                    "An item in a todo list."
                    {:id :todo/item
                     :name "Todo item"}
                    [item]
                    [:div (if (:done? item)
                            [:del (:description item)]
                            [:div (:description item)])]))

  (macroexpand-1 '(defc todo-item
                    [item]
                    [:div (if (:done? item)
                            [:del (:description item)]
                            [:div (:description item)])]))

  #__)

(defn with-components [env components]
  (assoc env
    :components (into {} (map (juxt :id identity)) components)))

