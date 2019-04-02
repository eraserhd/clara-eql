(ns net.eraserhead.clara-eql.core
  (:require
   [clara.rules :as r]
   [clara.rules.accumulators :as acc]
   [clara-eav.eav :refer :all]
   [clojure.spec.alpha :as s]
   [edn-query-language.core :as eql])
  (:import
   (clara_eav.eav EAV)))

(defrecord Candidate [query e])
(defrecord QueryResult [query e result])
(defrecord SingleAttributeQueryResult [query e a result])
(defrecord AttributeQueryResult [query e a result])

(defn- query-structure [query]
  (transduce
    (filter (comp #{:prop :join} :type))
    (fn 
      ([m] m)
      ([m child-query]
       (assoc m (:key child-query) (::variable child-query))))
    {}
    (:children query)))

(defn remove-nil-values [result]
  (into {} (remove (fn [[_ v]] (nil? v))) result))

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

(defn- candidate-join-subrule
  [query child-query]
  `(r/defrule ~(symbol (str (::rule-name child-query) "__candidates"))
     [:exists [Candidate (= ~'query '~(::rule-name query)) (= ~'e ?parent#)]]
     [:exists [EAV (= ~'e ?parent#) (= ~'a ~(:key child-query)) (= ~'v ?this#)]]
     ~'=>
     (r/insert! (->Candidate '~(::rule-name child-query) ?this#))))

(defn- candidate-rules [root]
  (cons
    `(r/defrule ~(symbol (str (::rule-name root) "__candidates"))
       ~@(::where root)
       ~'=>
       (r/insert! (->Candidate '~(::rule-name root) ~(::variable root))))
    (sequence (comp
                (filter (comp #{:join :root} :type))
                (mapcat #(for [child (:children %)] [% child]))
                (filter (fn [[parent child]]
                          (= :join (:type child))))
                (map (fn [[parent child]]
                       (candidate-join-subrule parent child))))
              (tree-seq :children :children root))))

(defn- attribute-rule
  [query child-query]
  (let [subrule-name        (::rule-name child-query)
        attribute-rule-name (symbol (str (name subrule-name) "__attribute"))
        attribute           (:key child-query)]
    `(r/defrule ~attribute-rule-name
       ~@(when-let [properties (::properties query)] [properties])
       [:exists [Candidate (= ~'query '~(::rule-name query)) (= ~'e ~(::variable query))]]
       [?many# ~'<- (acc/count) :from [EAV (= ~'e ~attribute) (= ~'a :db/cardinality) (= ~'v :db.cardinality/many)]]
       [?results# ~'<- (acc/all :result) :from [SingleAttributeQueryResult
                                                (= ~'query '~subrule-name)
                                                (= ~'e ~(::variable query))
                                                (= ~'a ~attribute)]]
       ~'=>
       (let [value# (if (zero? ?many#)
                      (first ?results#)
                      ?results#)]
         (r/insert! (->AttributeQueryResult '~subrule-name ~(::variable query) ~attribute value#))))))

(defn- attribute-rules [root]
  (sequence (comp
              (filter (comp #{:root :join} :type))
              (mapcat #(for [child (:children %)] [% child]))
              (filter (fn [[parent child]]
                        (#{:prop :join} (:type child))))
              (map (fn [[parent child]]
                     (attribute-rule parent child))))
            (tree-seq :children :children root)))

(defn- attribute-productions
  [from child-query]
  (let [{:keys [::rule-name :key ::variable]} child-query]
    `[AttributeQueryResult (= ~'query '~rule-name) (= ~'e ~from) (= ~'a ~key) (= ~'result ~variable)]))

(defn- join-rule
  [query]
  (let [{:keys [:type :children ::rule-name ::doc ::properties ::variable]} query
        child-productions (->> children
                            (filter (comp #{:prop :join} :type))
                            (map (partial attribute-productions variable)))]
    (case type
      :root `(r/defrule ~(symbol (name rule-name))
               ~@(when doc [doc])
               ~@(when properties [properties])
               [:exists [Candidate (= ~'query '~rule-name) (= ~'e ~variable)]]
               ~@child-productions
               ~'=>
               (let [~'result (remove-nil-values ~(query-structure query))]
                 (r/insert! (->QueryResult '~rule-name ~variable ~'result))))
      :join `(r/defrule ~(symbol (name rule-name))
               ~@(when doc [doc])
               ~@(when properties [properties])
               [:exists [Candidate (= ~'query '~rule-name) (= ~'e ~variable)]]
               ~@child-productions
               [EAV (= ~'e ?parent#) (= ~'a ~(:key query)) (= ~'v ~variable)]
               ~'=>
               (let [~'result (remove-nil-values ~(query-structure query))]
                 (r/insert! (->SingleAttributeQueryResult '~rule-name ?parent# ~(:key query) ~'result)))))))

(defn- join-rules [root]
  (sequence (comp
              (filter (comp #{:root :join} :type))
              (map join-rule))
            (tree-seq :children :children root)))

(defn- map-nodes [f node]
  (f (eql/transduce-children (map f) node)))

(defn- key->variable [kw]
  (symbol (str \? (namespace kw) \_ (name kw))))

(defn- nest-salience
  "To avoid retriggering, we prioritize deeper nodes over inner ones, since
  computing the deeper ones causes the inner ones to potentially recompute."
  [root salience]
  (-> root
    (assoc-in [::properties :salience] salience)
    (update :children (fn [children]
                        (mapv (fn [child]
                                (nest-salience child (inc salience)))
                              children)))))

(defn- add-variables [root from]
  (map-nodes (fn [node]
               (case (:type node)
                 :root         (assoc node ::variable from)
                 (:prop :join) (assoc node ::variable (key->variable (:key node)))
                 node))
             root))

(defn- add-parent-variables [root parent-variable]
  (-> root
    (assoc ::parent-variable parent-variable)
    (update :children (fn [children]
                        (mapv (fn [child]
                                (if (::variable root)
                                  (add-parent-variables child (::variable root))
                                  (add-parent-variables child parent-variable)))
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

(defn- add-rule-names [root rule-name]
  (map-nodes (fn [{:keys [::path] :as node}]
               (assoc node ::rule-name (reduce (fn [rule-name kw]
                                                (symbol (namespace rule-name)
                                                        (str (name rule-name) (name (key->variable kw)))))
                                               rule-name
                                               path)))
             root))

(defn- add-parent-rule-names [root rule-name]
  (-> root
    (assoc ::parent-rule-name rule-name)
    (update :children (partial mapv #(add-parent-rule-names % (::rule-name root))))))

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

(defn- prop-rule [query]
  (let [{:keys [::rule-name ::parent-rule-name ::parent-variable ::variable ::properties ::where]} query]
    [`(r/defrule ~(symbol (name rule-name))
        ~@(when properties [properties])
        [:exists [Candidate (= ~'query '~parent-rule-name) (= ~'e ~parent-variable)]]
        [:exists [EAV (= ~'e ~parent-variable) (= ~'a ~(:key query)) (= ~'v ~variable)]]
        ~'=>
        (r/insert! (->SingleAttributeQueryResult '~rule-name ~parent-variable ~(:key query) ~variable)))]))

(defn- prop-rules [root]
  (sequence (comp
              (filter (comp #{:prop} :type))
              (map prop-rule))
            (tree-seq :children :children root)))

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
        query          (-> (s/unform ::eql/query query)
                           eql/query->ast
                           (cond-> doc (assoc ::doc doc))
                           (cond-> properties (assoc ::properties properties))
                           (nest-salience (or (:salience properties) 0))
                           (add-variables from)
                           (add-parent-variables nil)
                           (add-paths [])
                           (add-rule-names qualified-name)
                           (add-parent-rule-names nil)
                           (add-wheres where from))]
    `(do
       ~@(candidate-rules query)
       ~@(prop-rules query)
       ~@(attribute-rules query)
       ~@(join-rules query))))
