(ns net.eraserhead.clara-eql
  (:require
   [clara.rules :as r]
   [clara.rules.accumulators :as acc]
   [clara-eav.eav :refer :all]
   [clojure.spec.alpha :as s]
   [clojure.walk :as walk]
   [edn-query-language.core :as eql])
  (:import
   (clara_eav.eav EAV)))

(defrecord QueryData [query root data])

(defn- key->variable
  [kw]
  (symbol (str \? (namespace kw) \_ (name kw))))

(defn prop-node-productions
  [eid-var query]
  (let [attr-var (:key query)
        val-var  (key->variable (:key query))]
    (if (:params query)
      `([~val-var ~'<- (acc/all :v) :from [EAV (= ~'e ~eid-var) (= ~'a ~attr-var)]])
      `([:or
         [EAV (= ~'e ~eid-var) (= ~'a ~attr-var) (= ~'v ~val-var)]
         [:not [EAV (= ~'e ~eid-var) (= ~'a ~attr-var)]]]))))

(defn- query-productions [eid-var query]
  (case (:type query)
    :root (mapcat (partial query-productions eid-var) (:children query))
    :prop (prop-node-productions eid-var query)))

(defn- query-structure [query]
  (case (:type query)
    :root (reduce
           (fn [m child-query]
             (assoc m (:key child-query) (query-structure child-query)))
           {}
           (:children query))
    :prop (key->variable (:key query))))

(defn remove-nil-values [data]
  (clojure.walk/postwalk
   (fn [x]
     (cond->> x
       (map? x)
       (reduce-kv
        (fn [m k v]
          (cond-> m v (assoc k v)))
        {})))
   data))

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
       (r/insert! (->QueryData '~qualified-name ~from (remove-nil-values ~(query-structure query)))))))
