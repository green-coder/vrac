(ns vrac.test.util)

(defn make-gensym
  "Provides a function which returns symbols consistently & deterministically."
  []
  (let [n (atom 0)]
    (fn gensym
      ([] (gensym "G__"))
      ([prefix-string]
       (symbol (str prefix-string (swap! n inc)))))))
