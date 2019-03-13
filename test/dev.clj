(ns dev
  (:require
   [clojure.walk]
   [zprint.core :as zp]
   [net.eraserhead.clara-eql.core :refer [defrule]]
   [midje.repl :refer :all]))

(defn- make-symbols-easier-to-read [code]
  (clojure.walk/postwalk
    (fn [x]
      (if (symbol? x)
        (symbol (second (re-matches #"^(?:.*\.)?([^\.]*)$" (name x))))
        x))
    code))

(defn- hack-for-quoted-symbols [code]
  (clojure.walk/postwalk
    (fn [x]
      (if (and (sequential? x)
               (= 2 (count x))
               (= 'quote (first x))
               (symbol? (second x)))
        (symbol (str \' (name (second x))))
        x))
    code))

(defn expand
  [rule-code]
  (let [rules (-> rule-code
                macroexpand-1
                make-symbols-easier-to-read
                hack-for-quoted-symbols
                rest)]
    (doseq [rule rules]
      (zp/zprint rule {:width 110
                       :fn-map {"defrule" :arg1-force-nl}})
      (println))))
