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
  [query where child-query]
  (case (:type child-query)
    :union
    nil
    (:prop :join)
    (let [subrule-name        (::rule-name child-query)
          attribute-rule-name (symbol (str (name subrule-name) "__attribute"))
          attribute           (:key child-query)]
      `(r/defrule ~attribute-rule-name
         ~@where
         [:or
          [:and
           [:not [EAV (= ~'e ~attribute) (= ~'a :db/cardinality) (= ~'v :db.cardinality/many)]]
           [SingleAttributeQueryResult
            (= ~'query '~subrule-name)
            (= ~'e ~(::variable query))
            (= ~'a ~attribute)
            (= ~'result ?result#)]]
          [:and
           [:not [EAV (= ~'e ~attribute) (= ~'a :db/cardinality) (= ~'v :db.cardinality/many)]]
           [:not [SingleAttributeQueryResult (= ~'query '~subrule-name) (= ~'e ~(::variable query)) (= ~'a ~attribute)]]]
          [:and
           [EAV (= ~'e ~attribute) (= ~'a :db/cardinality) (= ~'v :db.cardinality/many)]
           [?result# ~'<- (acc/all :result) :from [SingleAttributeQueryResult
                                                   (= ~'query '~subrule-name)
                                                   (= ~'e ~(::variable query))
                                                   (= ~'a ~attribute)]]]]
         ~'=>
         (r/insert! (->AttributeQueryResult '~subrule-name ~(::variable query) ~attribute ?result#))))))

(defn- attribute-productions
  [from child-query]
  (let [query (::rule-name child-query)
        attr (:key child-query)
        result (key->variable attr)]
    `[AttributeQueryResult (= ~'query '~query) (= ~'e ~from) (= ~'a ~attr) (= ~'result ~result)]))

(defn- prop-node-rule
  [query]
  (let [{:keys [::rule-name ::variable ::where]} query]
    [`(r/defrule ~(symbol (name rule-name))
        ~@where
        ~'=>
        (r/insert! (->QueryResult '~rule-name ~variable ~variable)))]))

(defn- rule-code
  [query where]
  (case (:type query)
    :prop
    (prop-node-rule query)
    :union
    nil
    :union-entry
    nil
    (:root :join)
    (concat
     (mapcat (fn [child-query]
               (when (#{:prop :join} (:type child-query))
                 (let [attr   (:key child-query)
                       where' (concat where [`[EAV (= ~'e ~(::variable query)) (= ~'a ~attr) (= ~'v ~(::variable child-query))]])]
                   (rule-code child-query where'))))
             (:children query))
     (map (partial attribute-rule query where) (:children query))
     [`(r/defrule ~(symbol (name (::rule-name query)))
         ~@(when-let [doc (::doc query)] [doc])
         ~@(when-let [properties (::properties query)] [properties])
         ~@where
         ~@(->> (:children query)
                (filter (comp #{:prop :join} :type))
                (map (partial attribute-productions (::variable query))))
         ~'=>
         (r/insert! (->QueryResult '~(::rule-name query) ~(::variable query) (remove-nil-values ~(query-structure query)))))])))

(defn- map-nodes [f node]
  (f (eql/transduce-children (map f) node)))

(defn- add-variables [root from]
  (map-nodes (fn [node]
               (case (:type node)
                 :root         (assoc node ::variable from)
                 (:prop :join) (assoc node ::variable (key->variable (:key node)))
                 node))
             root))

(defn- add-rule-names [root rule-name]
  (-> root
    (assoc ::rule-name rule-name)
    (update :children (fn [children]
                        (mapv (fn [child]
                                (case (:type child)
                                  (:prop :join) (add-rule-names child (subquery-name rule-name child))
                                  #_otherwise   (add-rule-names child rule-name)))
                              children)))))

(defn- add-paths [root path]
  (-> root
    (assoc ::path path)
    (update :children (fn [children]
                        (mapv (fn [child]
                                (case (:type child)
                                  (:prop :join) (add-paths child (conj path (:key child)))
                                  #_otherwise   (add-paths child path)))
                              children)))))

(defn- add-wheres [root where from]
  (map-nodes (fn [{:keys [::path] :as node}]
               (let [where (first (reduce (fn [[where from] kw]
                                            (let [vname (key->variable kw)]
                                              [(concat
                                                where
                                                `([EAV (= ~'e ~from) (= ~'a ~kw) (= ~'v ~vname)]))
                                               vname]))
                                          [where from]
                                          path))]
                 (assoc node ::where where)))
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
                           (add-variables from)
                           (add-rule-names qualified-name)
                           (add-paths [])
                           (add-wheres where from))]
    `(do ~@(rule-code query where))))
