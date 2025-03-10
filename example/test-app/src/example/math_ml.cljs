(ns example.math-ml
  (:require [vrac.web :as vw :refer [$]]))

(defn- sample1-article []
  ($ :article
     ($ :h2 "MathML sample 1")
     ($ :math {:display "block"}
        ($ :mrow
           ($ :mrow
              ($ :mo "|")
              ($ :mi "x")
              ($ :mo "|"))
           ($ :mo "=")
           ($ :mi "x"))
        ($ :mtext "\u00a0iff\u00a0")
        ($ :mrow
           ($ :mi "x")
           ($ :mo "≥")
           ($ :mn "0")))))

(defn- sample2-article []
  ($ :article
     ($ :h2 "MathML sample 2")
     ($ :math {:display "block"}
        ($ :mfrac
           ($ :mrow
              ($ :mi "a")
              ($ :mo "+")
              ($ :mn "2"))
           ($ :mrow
              ($ :mn "3")
              ($ :mo "−")
              ($ :mi "b"))))))

(defn math-ml-root []
  ($ :div
    ($ sample1-article)
    ($ sample2-article)))
