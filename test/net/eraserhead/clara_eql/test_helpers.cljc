(ns net.eraserhead.clara-eql.test-helpers
  (:require
   [clojure.walk]))

(defn sort-multi-values [result]
  (clojure.walk/postwalk
   (fn [x]
     (if (and (vector? x) (not (map-entry? x)))
       (->> x (map pr-str) sort (map read-string) vec)
       x))
   result))
