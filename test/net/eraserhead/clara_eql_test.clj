(ns net.eraserhead.clara-eql-test
  (:require
   [midje.sweet :refer :all]
   [clara.rules :as r]
   [clara-eav.eav :as eav]
   [net.eraserhead.clara-eql :refer :all])
  (:import
   (clara_eav.eav EAV)
   (net.eraserhead.clara_eql QueryData)))

(r/defquery query-results
  []
  [QueryData (= root ?root) (= query ?query) (= data ?data)])

(defrule basic-rule
  :query [:foo/uuid]
  :from ?eid
  :where
  [EAV (= e ?eid) (= a :foo/uuid)])

(defrule missing-property-rule
  :query [:foo/uuid :foo/missing]
  :from ?eid
  :where
  [EAV (= e ?eid) (= a :foo/uuid)])

(facts "about parse-rule"
  (fact "it generates one result for every match of `:where`"
    (let [session (-> (r/mk-session)
                      (r/insert (eav/->EAV 10 :foo/uuid "aaa"))
                      (r/fire-rules))]
      (r/query session query-results) => (contains {:?query `basic-rule
                                                    :?root 10
                                                    :?data {:foo/uuid "aaa"}})
      (fact "missing top-level properties don't prevent returning a result"
        (r/query session query-results) => (contains {:?query `missing-property-rule
                                                      :?root 10
                                                      :?data {:foo/uuid "aaa"}})))))
