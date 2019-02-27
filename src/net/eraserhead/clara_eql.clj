(ns net.eraserhead.clara-eql
  (:require
   [clara.rules :as r]
   [clara.rules.dsl :as dsl]
   [clara-eav.eav :refer :all]
   [clojure.spec.alpha :as s]
   [edn-query-language.core :as eql])
  (:import
   (clara_eav.eav EAV)))

(defrecord QueryData [query root data])

(defn- key->variable
  [kw]
  (symbol (str \? (namespace kw) \_ (name kw))))

(defn- query-productions [eid-var query]
  (case (:type query)
    :root (mapcat (partial query-productions eid-var) (:children query))
    :prop `([EAV (= ~'e ~eid-var) (= ~'a ~(:key query)) (= ~'v ~(key->variable (:key query)))])
    []))

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
        query (eql/query->ast (s/unform ::eql/query query))
        qualified-name (symbol (name (ns-name *ns*)) (name rule-name))
        productions (query-productions from query)]
    `(r/defrule ~rule-name
       ~@where
       ~@productions
       ~'=>
       (r/insert! (->QueryData '~qualified-name ~from {:foo/uuid ~(key->variable :foo/uuid)})))))
