(ns vrac.util
  (:require [clojure.string :as str]
            [camel-snake-kebab.core :as csk]))

(defn map-keys [m f & args]
  (into {}
        (map (fn [[k v]] [(apply f k args) v]))
        m))

(defn map-vals [m f & args]
  (into {}
        (map (fn [[k v]] [k (apply f v args)]))
        m))

(defn tag-id-class [kw]
  (let [[ns name] ((juxt namespace name) kw)
        tag-id-class (reduce (fn [acc part]
                               (case (subs part 0 1)
                                 "." (update acc :class conj (subs part 1))
                                 "#" (assoc acc :id (subs part 1))
                                 (assoc acc :tag part)))
                             {:tag "div"
                              :id nil
                              :class []}
                             (re-seq #"[#.]?[^#.]+" name))]
    (update tag-id-class :tag #(keyword ns %))))

; TODO: rename class -> classes
; TODO: Specify that classes are all strings.
; TODO: classes are internally handled using an ordered set.
(defn merge-id-class-with-props
  "Merges the id and classes with the props."
  [id class props]
  (assoc props
    :id (or (:id props) id)
    :class (let [cls (:class props)]
             (cond (nil? cls) class
                   (coll? cls) (into class cls)
                   :else (conj class cls)))))

#?(:cljs
   (defn ->html-attributes
     "Converts the vrac props to a js map to be used as attributes on an html tag."
     [props]
     (let [class (:class props)
           style (:style props)]
       (-> (dissoc props :class)
           (map-keys csk/->camelCase)
           (cond-> (seq class) (assoc :className (str/join " " class)))
           (cond-> style (assoc :style (map-keys style csk/->camelCase)))
           (clj->js)))))
