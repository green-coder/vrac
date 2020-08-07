(ns vrac.react.core
  (:require [vrac.util :refer [map-keys map-vals
                               tag-id-class
                               merge-id-class-with-props
                               #?(:cljs ->html-attributes)]]
            #?@(:cljs [["react" :as react]
                       ["react-dom" :as react-dom]])))

;; Interpretation

;; TODO: rethink those 2 problematic hacks:
;; - prop symbols as keywords in the props map
;;   * we should indeed use keywords, the props are symbols which map to kw-value pairs.
;; - props wrapped in {:vrac ...}
;;   * {:vrac ...} means 1 object to test for equality
;;   * #js {:prop1 cljs-data1, :prop2 cljs-data2, ...} means better compatibility with React?
;;     .. not if we know if we call vrac or react.))
#?(:cljs
    (defn- template-renderer
      "Returns a function which interprets a template with some props
       and which returns some React's VDOM."
      [env-atom component-id]
      (let [{:keys [create-element] :as env} @env-atom
            parsed-template (-> env :components component-id :parsed-template)
            render
            (fn render [env context [kw val :as parsed-template]]
              (case kw
                (:nil :boolean :number :string :keyword) val
                :symbol (context (keyword val))
                :map (into {} (map (fn [[k v]]
                                     [(render env context k)
                                      (render env context v)])) val)
                :get-kw (let [{:keys [keyword valuable]} val]
                          (keyword (if (= valuable [:nil nil])
                                     (context nil)
                                     (render env context valuable))))
                :if (let [{:keys [cond then else]} val]
                      (render env context
                              (if (render env context cond)
                                then
                                else)))
                :when (let [{:keys [cond then]} val]
                        (when (render env context cond)
                          (render env context then)))
                :let (let [{:keys [bindings body]} val
                           inner-context (reduce (fn [context {:keys [symbol valuable]}]
                                                   (assoc context
                                                     (keyword (name symbol)) (render env context valuable)))
                                                 context
                                                 bindings)]
                       (render env inner-context body))
                :for (let [{:keys [bindings body]} val
                           for (fn for [context [binding & next-bindings]]
                                 (if (nil? binding)
                                   [(render env context body)]
                                   (let [{:keys [symbol valuable]} binding
                                         coll (render env context valuable)]
                                     (mapcat (fn [val]
                                               (for (assoc context (keyword (name symbol)) val) next-bindings))
                                             coll))))]
                       (-> (for context bindings)
                           (with-meta {:inline true})))
                :comp (let [{:keys [keyword props children]} val
                            rendered-props (map-vals props (partial render env context))
                            {:keys [tag id class]} (tag-id-class keyword)
                            component-id tag
                            merged-props (cond->> (merge-id-class-with-props id class rendered-props)
                                           (::is-root-comp? env) (merge-id-class-with-props (:id context nil)
                                                                                            (:class context [])))
                            [component props] (if (simple-keyword? keyword) ; html node?
                                                [(name tag) (->html-attributes merged-props)]
                                                [(-> @env-atom :components component-id :react-render)
                                                 #js {:vrac merged-props}])
                            children (into []
                                           (mapcat (fn [child]
                                                     (let [rendered (render (dissoc env ::is-root-comp?) context child)]
                                                       (if (some-> (meta rendered) :inline)
                                                         rendered
                                                         [rendered]))))
                                           children)]
                        (apply create-element
                               component
                               props
                               children))
                (:dsl :valuable) (render env context val)))]
         (fn [wrapped-props]
           (println (str "** Render Vrac component " component-id))
           (let [env {::is-root-comp? true}
                 props (.-vrac wrapped-props)]
             (render env props parsed-template))))))

; TODO: try to remember how to selectively re-render 1 instance of a React component.
; I found the project, it's via a function called something like forceUpdate.

#?(:cljs
   (defn- vrac=
     "Compares props for vrac components - they are wrapped so that React won't change them."
     [old new]
     (= (.-vrac old) (.-vrac new))))

#?(:cljs
   (defn- with-display-name [render-fn name]
     (set! (.-displayName render-fn) name)
     render-fn))

#?(:cljs
   (defn with-react-renderer [env]
     (let [env (assoc env :create-element react/createElement)
           env-atom (atom env)
           react-render (fn [comp]
                          (-> (template-renderer env-atom (:id comp))
                              (with-display-name (str ((some-fn :name :id) comp)))
                              (react/memo (fn [old new]
                                            (do (prn "vrac=" (:id comp))
                                                (vrac= old new))))))
           env (update env :components
                       map-vals (fn [comp]
                                  (assoc comp
                                     :react-render (react-render comp))))]
       (reset! env-atom env)
       env)))

#?(:cljs
   (defn render [env component-id data-tree mounting-element-id]
     (react-dom/render
       (react/createElement
         (-> env :components component-id :react-render)
         #js {:vrac data-tree})
       (js/document.getElementById mounting-element-id))))
