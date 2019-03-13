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

(r/defrule single-attribute-query-results
  [EAV (= e ?e) (= a ?a) (= v ?v)]
  [QueryResult (= query ?query) (= e ?v) (= result ?result)]
  =>
  (r/insert! (->SingleAttributeQueryResult ?query ?e ?a ?result)))

(defn- key->variable [kw]
  (symbol (str \? (namespace kw) \_ (name kw))))

(defn- subquery-name [qualified-name query]
  (symbol (namespace qualified-name)
          (str (name qualified-name) (name (key->variable (:key query))))))

(defn- query-structure [query]
  (reduce (fn [m child-query]
            (case (:type child-query)
              (:prop :join)
              (assoc m (:key child-query) (key->variable (:key child-query)))
              nil))
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

(defn- attribute-rule
  [qualified-name from where child-query]
  (case (:type child-query)
    :union
    nil
    (:prop :join)
    (let [subrule-name        (subquery-name qualified-name child-query)
          attribute-rule-name (symbol (str (name subrule-name) "__attribute"))
          attribute           (:key child-query)]
      `(r/defrule ~attribute-rule-name
         ~@where
         [:or
          [:and
           [:not [EAV (= ~'e ~attribute) (= ~'a :db/cardinality) (= ~'v :db.cardinality/many)]]
           [SingleAttributeQueryResult
            (= ~'query '~subrule-name)
            (= ~'e ~from)
            (= ~'a ~attribute)
            (= ~'result ?result#)]]
          [:and
           [:not [EAV (= ~'e ~attribute) (= ~'a :db/cardinality) (= ~'v :db.cardinality/many)]]
           [:not [SingleAttributeQueryResult (= ~'query '~subrule-name) (= ~'e ~from) (= ~'a ~attribute)]]]
          [:and
           [EAV (= ~'e ~attribute) (= ~'a :db/cardinality) (= ~'v :db.cardinality/many)]
           [?result# ~'<- (acc/all :result) :from [SingleAttributeQueryResult
                                                   (= ~'query '~subrule-name)
                                                   (= ~'e ~from)
                                                   (= ~'a ~attribute)]]]]
         ~'=>
         (r/insert! (->AttributeQueryResult '~subrule-name ~from ~attribute ?result#))))))

(defn- attribute-productions
  [qualified-name from child-query]
  (let [query (subquery-name qualified-name child-query)
        attr (:key child-query)
        result (key->variable attr)]
    `[AttributeQueryResult (= ~'query '~query) (= ~'e ~from) (= ~'a ~attr) (= ~'result ~result)]))

(defn- prop-node-rule
  [query qualified-name where]
  (let [{:keys [::variable]} query]
    [`(r/defrule ~(symbol (name qualified-name))
        ~@where
        ~'=>
        (r/insert! (->QueryResult '~qualified-name ~variable ~variable)))]))

(defn- rule-code
  [qualified-name query where]
  (case (:type query)
    :prop
    (prop-node-rule query qualified-name where)
    :union
    nil
    :union-entry
    nil
    (:root :join)
    (concat
     (mapcat (fn [child-query]
               (when (#{:prop :join} (:type child-query))
                 (let [qualified-name' (subquery-name qualified-name child-query)
                       attr            (:key child-query)
                       where'          (concat where [`[EAV (= ~'e ~(::variable query)) (= ~'a ~attr) (= ~'v ~(::variable child-query))]])]
                   (rule-code qualified-name' child-query where'))))
             (:children query))
     (map (partial attribute-rule qualified-name (::variable query) where) (:children query))
     [`(r/defrule ~(symbol (name qualified-name))
         ~@(when-let [doc (::doc query)] [doc])
         ~@(when-let [properties (::properties query)] [properties])
         ~@where
         ~@(->> (:children query)
                (filter (comp #{:prop :join} :type))
                (map (partial attribute-productions qualified-name (::variable query))))
         ~'=>
         (r/insert! (->QueryResult '~qualified-name ~(::variable query) (remove-nil-values ~(query-structure query)))))])))

(defn- map-nodes [f node]
  (f (eql/transduce-children (map f) node)))

(defn- add-variables [root from]
  (map-nodes (fn [node]
               (case (:type node)
                 :root         (assoc node ::variable from)
                 (:prop :join) (assoc node ::variable (key->variable (:key node)))
                 node))
             root))

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
        qualified-name (symbol (name (ns-name *ns*)) (name rule-name))
        query          (-> (eql/query->ast (s/unform ::eql/query query))
                           (cond-> doc (assoc ::doc doc))
                           (cond-> properties (assoc ::properties properties))
                           (add-variables from))]
    `(do ~@(rule-code qualified-name query where))))
