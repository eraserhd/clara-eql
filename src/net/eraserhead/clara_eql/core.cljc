(ns net.eraserhead.clara-eql.core
  (:require
   [clara.rules :as r]
   [clara.rules.accumulators :as acc]
   [clara-eav.eav]
   [clojure.spec.alpha :as s]
   [edn-query-language.core :as eql])
  #?(:clj  (:import  (clara_eav.eav EAV))
     :cljs (:require [clara-eav.eav :refer [EAV]])))

(defrecord Candidate [query e])
(defrecord QueryResult [query e result])
(defrecord SingleAttributeQueryResult [query e a result])
(defrecord AttributeQueryResult [query e a result])

(defn- cljs-env?
  "Whether this &env represents a ClojureScript macro expansion."
  [env]
  (boolean (:ns env)))

(defn- fact-type
  "Return the fully qualified symbols for a fact type.

  We need to expand the symbols differently when expanding the macro for Clojure or
  ClojureScript."
  [env sym]
  (if (cljs-env? env)
    (if (= 'EAV sym)
      'clara-eav.eav/EAV
      (symbol "net.eraserhead.clara-eql.core" (name sym)))
    (if (= 'EAV sym)
      'clara_eav.eav.EAV
      (symbol (str "net.eraserhead.clara_eql.core." (name sym))))))

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
  [env child]
  `(r/defrule ~(symbol (str (::rule-name child) "__candidates"))
     {:salience ~(::candidate-salience child)}
     [:exists [~(fact-type env 'Candidate) (= ~'query '~(::parent-rule-name child)) (= ~'e ?parent#)]]
     [:exists [~(fact-type env 'EAV) (= ~'e ?parent#) (= ~'a ~(:key child)) (= ~'v ?this#)]]
     ~'=>
     (r/insert! (->Candidate '~(::rule-name child) ?this#))))

(defn- candidate-rules [env root]
  (cons
    `(r/defrule ~(symbol (str (::rule-name root) "__candidates"))
       {:salience ~(::candidate-salience root)}
       ~@(::where root)
       ~'=>
       (r/insert! (->Candidate '~(::rule-name root) ~(::variable root))))
    (sequence (comp
                (filter (comp #{:join :root} :type))
                (mapcat :children)
                (filter (comp #{:join} :type))
                (map (partial candidate-join-subrule env)))
              (tree-seq :children :children root))))

(defn- attribute-rule
  [env child]
  (let [subrule-name        (::rule-name child)
        attribute-rule-name (symbol (str (name subrule-name) "__attribute"))
        attribute           (:key child)]
    `(r/defrule ~attribute-rule-name
       ~@(when-let [properties (::properties child)] [properties])
       [:exists [~(fact-type env 'Candidate) (= ~'query '~(::parent-rule-name child)) (= ~'e ~(::parent-variable child))]]
       [?many# ~'<- (acc/count) :from [~(fact-type env 'EAV) (= ~'e ~attribute) (= ~'a :db/cardinality) (= ~'v :db.cardinality/many)]]
       [?results# ~'<- (acc/all :result) :from [~(fact-type env 'SingleAttributeQueryResult)
                                                (= ~'query '~subrule-name)
                                                (= ~'e ~(::parent-variable child))
                                                (= ~'a ~attribute)]]
       ~'=>
       (let [value# (if (zero? ?many#)
                      (first ?results#)
                      ?results#)]
         (r/insert! (->AttributeQueryResult '~subrule-name ~(::parent-variable child) ~attribute value#))))))

(defn- attribute-rules [env root]
  (sequence (comp
              (filter (comp #{:root :join} :type))
              (mapcat :children)
              (filter (comp #{:prop :join} :type))
              (map (partial attribute-rule env)))
            (tree-seq :children :children root)))

(defn- attribute-productions
  [env from child-query]
  (let [{:keys [::rule-name :key ::variable]} child-query]
    `[~(fact-type env 'AttributeQueryResult) (= ~'query '~rule-name) (= ~'e ~from) (= ~'a ~key) (= ~'result ~variable)]))

(defn- join-rule
  [env query]
  (let [{:keys [:type :children ::rule-name ::doc ::properties ::variable]} query
        child-productions (->> children
                            (filter (comp #{:prop :join} :type))
                            (map (partial attribute-productions env variable)))]
    (case type
      :root `(r/defrule ~(symbol (name rule-name))
               ~@(when doc [doc])
               ~@(when properties [properties])
               [:exists [~(fact-type env 'Candidate) (= ~'query '~rule-name) (= ~'e ~variable)]]
               ~@child-productions
               ~'=>
               (let [~'result (remove-nil-values ~(query-structure query))]
                 (r/insert! (->QueryResult '~rule-name ~variable ~'result))))
      :join `(r/defrule ~(symbol (name rule-name))
               ~@(when doc [doc])
               ~@(when properties [properties])
               [:exists [~(fact-type env 'Candidate) (= ~'query '~rule-name) (= ~'e ~variable)]]
               ~@child-productions
               [~(fact-type env 'EAV) (= ~'e ?parent#) (= ~'a ~(:key query)) (= ~'v ~variable)]
               ~'=>
               (let [~'result (remove-nil-values ~(query-structure query))]
                 (r/insert! (->SingleAttributeQueryResult '~rule-name ?parent# ~(:key query) ~'result)))))))

(defn- join-rules [env root]
  (sequence (comp
              (filter (comp #{:root :join} :type))
              (map (partial join-rule env)))
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

(defn- nest-candidate-salience
  "Candidates should fire in the order opposite of other rules (outermost
  first)."
  [root salience]
  (-> root
    (assoc ::candidate-salience salience)
    (update :children (fn [children]
                        (mapv (fn [child]
                                (nest-candidate-salience child (dec salience)))
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

(defn- prop-rule [env query]
  (let [{:keys [::rule-name ::parent-rule-name ::parent-variable ::variable ::properties]} query]
    `(r/defrule ~(symbol (name rule-name))
       ~@(when properties [properties])
       [:exists [~(fact-type env 'Candidate) (= ~'query '~parent-rule-name) (= ~'e ~parent-variable)]]
       [:exists [~(fact-type env 'EAV) (= ~'e ~parent-variable) (= ~'a ~(:key query)) (= ~'v ~variable)]]
       ~'=>
       (r/insert! (->SingleAttributeQueryResult '~rule-name ~parent-variable ~(:key query) ~variable)))))

(defn- prop-rules [env root]
  (sequence (comp
              (filter (comp #{:prop} :type))
              (map (partial prop-rule env)))
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
                           (assoc ::where where)
                           (nest-salience (or (:salience properties) 0))
                           (nest-candidate-salience (+ 35 (or (:salience properties) 0)))
                           (add-variables from)
                           (add-parent-variables nil)
                           (add-paths [])
                           (add-rule-names qualified-name)
                           (add-parent-rule-names nil))]
    `(do
       ~@(candidate-rules &env query)
       ~@(prop-rules &env query)
       ~@(attribute-rules &env query)
       ~@(join-rules &env query))))
