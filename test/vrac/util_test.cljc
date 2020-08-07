(ns vrac.util-test
  (:require #?(:clj  [clojure.test :refer [deftest testing is are]]
               :cljs [cljs.test :refer [deftest testing is are] :include-macros true])
            [vrac.util :as vu]))

(deftest tag-id-class-test
  (are [kw result]
    (= (#'vu/tag-id-class kw) result)

    :. {:tag :div
        :id nil
        :class []}

    :.class1 {:tag :div
              :id nil
              :class ["class1"]}

    :#the-one {:tag :div
               :id "the-one"
               :class []}

    :h1.class1#the-one.class2 {:tag :h1
                               :id "the-one"
                               :class ["class1" "class2"]}

    :todo/list.class1#the-one.class2 {:tag :todo/list
                                      :id "the-one"
                                      :class ["class1" "class2"]}

    :my.todo/list.class1#the-one.class2 {:tag :my.todo/list
                                         :id "the-one"
                                         :class ["class1" "class2"]}))

(deftest merge-id-class-with-props-test
  (are [id class props result]
    (= (#'vu/merge-id-class-with-props id class props) result)

    nil [] {}
    {:class []
     :id nil}

    "app" [] {}
    {:class []
     :id "app"}

    "app" [] {:id "the-one"}
    {:class []
     :id "the-one"}

    "app" ["green" "big"] {:class "red"}
    {:class ["green" "big" "red"]
     :id "app"}

    "app" ["green" "big"] {:class ["red" "yellow"]}
    {:class ["green" "big" "red" "yellow"]
     :id "app"}

    "app" [] {:style {:background-color "blue"}}
    {:class []
     :id "app"
     :style {:background-color "blue"}}))
