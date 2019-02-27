(ns net.eraserhead.clara-eql
  (:require
   [clara.rules :as r]
   [clara.rules.dsl :as dsl]
   [edn-query-language.core :as eql]))

(defrecord QueryData [query root data])

(defmacro defrule [rule-name & body]
  (assert (= :query (first body)) "rule must start with :query")
  (let [query (second body)
        _ (assert (= :from (nth body 2)) "rule missing :from in third position")
        from (nth body 3)
        _ (assert (symbol? from) ":from value is not a symbol")
        where (drop 5 body)
        qualified-name (symbol (name (ns-name *ns*)) (name rule-name))
        rule-body (concat
                   where
                   ['=>
                    `(r/insert! (->QueryData '~qualified-name ~from {}))])]
    `(def ~(vary-meta rule-name assoc :rule true)
       ~(dsl/build-rule rule-name rule-body (meta &form)))))
