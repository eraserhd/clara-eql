(ns net.eraserhead.clara-eql.core.basic-rule-test
  (:require
   [clara.rules :as r]
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
  (let [rule-name (symbol (str (ns-name this-ns)) (str (second rule)))
        session (-> (r/mk-session (ns-name this-ns))
                    (r/insert-all (map (partial apply eav/->EAV) facts))
                    (r/fire-rules)
                    t/dump-facts)
        results (map #(update % :?result t/sort-multi-values)
                     (r/query session query-results :?query rule-name))]
    (assert (= 1 (count results))
            (str "found " (count results) " results: " (pr-str results)))
    (:?result (first results))))

(defrule basic-rule
  "Some basic rule"
  {:salience 100}
  :query [:foo/uuid]
  :from ?eid
  :where
  [EAV (= e ?eid) (= a :foo/uuid)])

(deftest t-defrule-basic-rule
  (testing "about single-cardinality keys"
    (is (= {:foo/uuid "aaa"}
           (check
             '(defrule basic-rule
                "Some basic rule"
                {:salience 100}
                :query [:foo/uuid]
                :from ?eid
                :where
                [EAV (= e ?eid) (= a :foo/uuid)])
             [[:r :foo/uuid "aaa"]]))
        "returns a result when all values are present")))
