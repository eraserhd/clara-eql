(ns net.eraserhead.clara-eql.core
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

(defn- key->variable [kw]
  (symbol (str \? (namespace kw) \_ (name kw))))

(defn prop-node-productions [eid-var query]
  (let [attr-var (:key query)
        val-var  (key->variable (:key query))]
    `([:or
       [:and
        [EAV (= ~'e ~attr-var) (= ~'a :db/cardinality) (= ~'v :db.cardinality/many)]
        [~val-var ~'<- (acc/all :v) :from [EAV (= ~'e ~eid-var) (= ~'a ~attr-var)]]]
       [:and
        [:not [EAV (= ~'e ~attr-var) (= ~'a :db/cardinality) (= ~'v :db.cardinality/many)]]
        [:or
         [EAV (= ~'e ~eid-var) (= ~'a ~attr-var) (= ~'v ~val-var)]
         [:not [EAV (= ~'e ~eid-var) (= ~'a ~attr-var)]]]]])))

(defn- query-productions [qualified-name eid-var query]
  (case (:type query)
    :root (mapcat (partial query-productions eid-var) (:children query))
    :prop (prop-node-productions eid-var query)
    :join (let [attr-var (:key query)
                val-var (key->variable (:key query))]
            `([:or
               [EAV (= ~'e ~eid-var) (= ~'a ~attr-var) (= ~'v ~val-var)]
               [:not [EAV (= ~'e ~eid-var) (= ~'a ~attr-var)]]]
              ~@(mapcat (partial query-productions qualified-name val-var) (:children query))))))

(defn- query-structure [query]
  (case (:type query)
    (:root :join) (reduce
                   (fn [m child-query]
                     (assoc m (:key child-query) (query-structure child-query)))
                   {}
                   (:children query))
    :prop         (key->variable (:key query))))

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

(s/def ::variable (s/and simple-symbol? #(= \? (get (name %) 0))))

(s/def ::defrule-args
  (s/cat :rule-name  symbol?
         :doc        (s/? string?)
         :properties (s/? map?)
         :query-kw   #{:query}
         :query      ::eql/query
         :from-kw    #{:from}
         :from       ::variable
         :where-kw   #{:where}
         :where      (s/+ any?)))

(defn- rule-code
  [qualified-name query from where doc properties]
  (let [productions (mapcat (partial query-productions qualified-name from) (:children query))]
    (concat
     (mapcat (fn [child-query]
               (when (= :join (:type child-query))
                 (rule-code (symbol (namespace qualified-name)
                                    (str (name qualified-name) (name (key->variable (:key child-query)))))
                            child-query
                            (key->variable (:key child-query))
                            (concat
                             where
                             [`[EAV (= ~'e ~from) (= ~'a ~(:key child-query)) (= ~'v ~(key->variable (:key child-query)))]])
                            doc
                            properties)))
             (:children query))
     [`(r/defrule ~(symbol (name qualified-name))
         ~doc
         ~properties
         ~@where
         ~@productions
         ~'=>
         (r/insert! (->QueryData '~qualified-name ~from (remove-nil-values ~(query-structure query)))))])))

(s/fdef defrule
  :args ::defrule-args)

(defmacro defrule
  "Define a Clara rule to tally an eql query

  For example:

    (defrule sample-rule
      \"Find results such as {:foo/uuid ... :foo/bar {:bar/name \\\"aname\\\"}}\"
      :query [:foo/uuid {:foo/bar [:bar/name]}]
      :from ?eid
      :where
      [EAV (= e ?eid) (= a :foo/uuid)])

  Results are insert in QueryData facts with the following fields:

    query - A fully-qualified symbol naming the query (e.g. sample-ns/sample-rule)
    root  - The root from which the data was pulled (the values of ?eid above)
    data  - The resulting query data.
  "
  [rule-name & body]
  (let [{:keys [query from where doc properties]}
        (s/conform ::defrule-args (cons rule-name body))
        query          (eql/query->ast (s/unform ::eql/query query))
        qualified-name (symbol (name (ns-name *ns*)) (name rule-name))
        doc            (or doc "")
        properties     (or properties {})]
    `(do ~@(rule-code qualified-name query from where doc properties))))
