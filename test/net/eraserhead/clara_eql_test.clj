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
  :query []
  :from ?eid
  :where
  [EAV (= e ?eid) (= a :foo/uuid)])

(facts "about parse-rule"
  (fact "it generates one result for every match of `:where`"
    (let [session (-> (r/mk-session)
                      (r/insert (eav/->EAV 10 :foo/uuid "aaa"))
                      (r/fire-rules))]
      (r/query session query-results) => [{:?query `basic-rule, :?root 10, :?data {}}])))
