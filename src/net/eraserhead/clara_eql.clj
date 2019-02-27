(ns net.eraserhead.clara-eql
  (:require
   [clara.rules :as r]
   [clara.rules.dsl :as dsl]
   [clojure.spec.alpha :as s]
   [edn-query-language.core :as eql]))

(defrecord QueryData [query root data])

(s/def ::defrule-args
  (s/cat :rule-name symbol?
         :query-kw  #{:query}
         :query     ::eql/query
         :from-kw   #{:from}
         :from      symbol?
         :where-kw  #{:where}
         :where     (s/+ any?)))

(s/fdef defrule
  :args ::defrule-args)

(defmacro defrule [rule-name & body]
  (let [{:keys [query from where]} (s/conform ::defrule-args (cons rule-name body))
        qualified-name (symbol (name (ns-name *ns*)) (name rule-name))
        rule-body (concat
                   where
                   ['=>
                    `(r/insert! (->QueryData '~qualified-name ~from {}))])]
    `(def ~(vary-meta rule-name assoc :rule true)
       ~(dsl/build-rule rule-name rule-body (meta &form)))))
