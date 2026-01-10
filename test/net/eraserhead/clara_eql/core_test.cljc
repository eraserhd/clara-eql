(ns net.eraserhead.clara-eql.core-test
  (:require
   [clara.rules :as r]
   [clara.rules.accumulators :as acc]
   [clara-eav.eav :as eav]
   [clojure.test :refer [deftest testing is]]
   [net.eraserhead.clara-eql.core :refer :all]
   [net.eraserhead.clara-eql.test-helpers :as t])
  (:import
   (clara_eav.eav EAV)
   (net.eraserhead.clara_eql.core Candidate QueryResult)))

(r/defquery query-results
  [:?query]
  [QueryResult (= e :r) (= query ?query) (= result ?result)])

(def ^:private this-ns *ns*)

(defn- check [rule facts]
  ;; Unmap other rules first to make dump-facts nice
  (doseq [[sym var] (ns-publics this-ns)
          :when (:rule (meta var))]
    (ns-unmap this-ns sym))
  (binding [*ns* this-ns]
    (eval rule))
  (let [rule-name (symbol (str (ns-name this-ns)) (str (second rule)))
        session (-> (r/mk-session 'net.eraserhead.clara-eql.core-test)
                    (r/insert-all (map (partial apply eav/->EAV) facts))
                    (r/fire-rules)
                    t/dump-facts)
        results (map #(update % :?result t/sort-multi-values)
                     (r/query session query-results :?query rule-name))]
    (assert (= 1 (count results))
            (str "found " (count results) " results: " (pr-str results)))
    (:?result (first results))))

(deftest t-defrule-many-valued-join
  (testing "about joins"
    (is (= {:foo/many-valued [{:bar/name "b11"}
                              {:bar/name "b12"}]}
           (check
             '(defrule many-valued-join
                :query [{:foo/many-valued [:bar/name]}]
                :from ?eid
                :where
                [EAV (= e ?eid) (= a :foo/uuid) (= v "aaa")])
             [[:foo/many-valued :db/cardinality :db.cardinality/many]
              [:r :foo/uuid "aaa"]
              [:r :foo/many-valued 11]
              [:r :foo/many-valued 12]
              [11 :bar/name "b11"]
              [12 :bar/name "b12"]]))
        "returns collections for many-valued nested join values")))


(deftest t-defrule-many-valued-join2
  (testing "about joins"
    ;; This was producing twice as many `{:bar/name "bXX"}` maps because
    ;; the join rules were generating a result for each root times each
    ;; entity, instead of just for each entity.
    (is (= {:foo/many-valued [{:bar/name "b11"}
                              {:bar/name "b12"}]}
           (check
             '(defrule many-valued-join2
                :query [{:foo/many-valued [:bar/name]}]
                :from ?eid
                :where
                [EAV (= e ?eid) (= a :foo/uuid)])
             [[:foo/many-valued :db/cardinality :db.cardinality/many]
              [:r :foo/uuid "aaa"]
              [:r :foo/many-valued 11]
              [:r :foo/many-valued 12]
              [99 :foo/uuid "bbb"]
              [99 :foo/many-valued 11]
              [99 :foo/many-valued 12]
              [11 :bar/name "b11"]
              [12 :bar/name "b12"]]))
        "regression: shared subtrees aren't multiplied")))

(deftest t-defrule
  (testing "about unions")
    ;(future-fact "returns values from all branches of the union"))
  (testing "about idents"))
    ;(future-fact "returns values from the specified object")))
