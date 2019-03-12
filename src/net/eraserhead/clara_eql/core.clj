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

(defrecord QueryResult [query e result])
(defrecord SingleAttributeQueryResult [query e a result])
(defrecord AttributeQueryResult [query e a result])

(r/defrule foo
  [EAV (= e ?e) (= a ?a) (= v ?v)]
  [QueryResult (= query ?query) (= e ?v) (= result ?result)]
  =>
  (r/insert! (->SingleAttributeQueryResult ?query ?e ?a ?result)))

(defn- key->variable [kw]
  (symbol (str \? (namespace kw) \_ (name kw))))

(defn- subquery-name [qualified-name query]
  (symbol (namespace qualified-name)
          (str (name qualified-name) (name (key->variable (:key query))))))

(defn- prop-node-productions [eid-var query]
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

(defn- join-node-productions [qualified-name eid-var query]
  (let [attr-var (:key query)
        val-var (key->variable (:key query))
        subquery-name (subquery-name qualified-name query)]
    `([:or
       [:and
        [EAV (= ~'e ~eid-var) (= ~'a ~attr-var) (= ~'v ?root#)]
        [QueryResult (= ~'e ?root#) (= ~'query '~subquery-name) (= ~'result ~val-var)]]
       [:not
        [:and
         [EAV (= ~'e ~eid-var) (= ~'a ~attr-var) (= ~'v ?root#)]
         [QueryResult (= ~'e ?root#) (= ~'query '~subquery-name)]]]])))

(defn- query-productions [qualified-name eid-var query]
  (case (:type query)
    :root (mapcat (partial query-productions eid-var) (:children query))
    :prop (prop-node-productions eid-var query)
    :join (join-node-productions qualified-name eid-var query)))

(defn- query-structure [query]
  (reduce (fn [m child-query]
            (assoc m (:key child-query) (key->variable (:key child-query))))
          {}
          (:children query)))

(defn remove-nil-values [result]
  (clojure.walk/postwalk
   (fn [x]
     (cond->> x
       (map? x)
       (reduce-kv
        (fn [m k v]
          (cond-> m v (assoc k v)))
        {})))
   result))

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
  (case (:type query)
    :prop
    [`(r/defrule ~(symbol (name qualified-name))
        ~@where
        ~'=>
        (r/insert! (->QueryResult '~qualified-name ~from ~from)))]
    (:root :join)
    (concat
     (mapcat (fn [child-query]
               (rule-code (subquery-name qualified-name child-query)
                          child-query
                          (key->variable (:key child-query))
                          (concat
                           where
                           [`[EAV (= ~'e ~from) (= ~'a ~(:key child-query)) (= ~'v ~(key->variable (:key child-query)))]])
                          doc
                          properties))
             (:children query))
     (map (fn [child-query]
            `(r/defrule ~(symbol (str (name (subquery-name qualified-name child-query)) "__attribute"))
               ~@where
               [:or
                [:and
                 [:not [EAV (= ~'e ~(:key child-query)) (= ~'a :db/cardinality) (= ~'v :db.cardinality/many)]]
                 [SingleAttributeQueryResult
                  (= ~'query '~(subquery-name qualified-name child-query))
                  (= ~'e ~from)
                  (= ~'a ~(:key child-query))
                  (= ~'result ?result#)]]
                [:and
                 [:not [EAV (= ~'e ~(:key child-query)) (= ~'a :db/cardinality) (= ~'v :db.cardinality/many)]]
                 [:not [SingleAttributeQueryResult
                        (= ~'query '~(subquery-name qualified-name child-query))
                        (= ~'e ~from)
                        (= ~'a ~(:key child-query))]]]
                [:and
                 [EAV (= ~'e ~(:key child-query)) (= ~'a :db/cardinality) (= ~'v :db.cardinality/many)]
                 [?result# ~'<- (acc/all :result) :from [SingleAttributeQueryResult
                                                         (= ~'query '~(subquery-name qualified-name child-query))
                                                         (= ~'e ~from)
                                                         (= ~'a ~(:key child-query))]]]]
               ~'=>
               (r/insert! (->AttributeQueryResult '~(subquery-name qualified-name child-query)
                                                  ~from
                                                  ~(:key child-query)
                                                  ?result#))))
          (:children query))
     [`(r/defrule ~(symbol (name qualified-name))
         ~doc
         ~properties
         ~@where
         ~@(map (fn [child-query]
                  `[AttributeQueryResult
                    (= ~'query '~(subquery-name qualified-name child-query))
                    (= ~'e ~from)
                    (= ~'a ~(:key child-query))
                    (= ~'result ~(key->variable (:key child-query)))])
                (:children query))
         ~'=>
         (r/insert! (->QueryResult '~qualified-name ~from (remove-nil-values ~(query-structure query)))))])))

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

  Results are insert in QueryResult facts with the following fields:

    query  - A fully-qualified symbol naming the query (e.g. sample-ns/sample-rule)
    root   - The root from which the result was pulled (the values of ?eid above)
    result - The resulting query data.
  "
  [rule-name & body]
  (let [{:keys [query from where doc properties]}
        (s/conform ::defrule-args (cons rule-name body))
        query          (eql/query->ast (s/unform ::eql/query query))
        qualified-name (symbol (name (ns-name *ns*)) (name rule-name))
        doc            (or doc "")
        properties     (or properties {})]
    `(do ~@(rule-code qualified-name query from where doc properties))))
