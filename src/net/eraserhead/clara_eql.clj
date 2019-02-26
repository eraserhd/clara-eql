(ns net.eraserhead.clara-eql
  (:require
   [clara.rules :as r]
   [clara.rules.dsl :as dsl]))

(defrecord QueryData [query root data])

(defn parse-rule*
  [query from where properties env]
  (dsl/parse-rule* where `(r/insert! (->QueryData nil nil nil)) properties env))

(defmacro parse-rule
  ([query from where]
   (parse-rule* query from where nil &env))
  ([query from where properties]
   (parse-rule* query from where properties &env)))
