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

(facts "about parse-rule"
  (fact "it generates one result for every match of `:where`"
    (let [r (parse-rule [] ?eid ([EAV (= e ?eid) (= a :foo/uuid)]))
          session (-> (r/mk-session 'net.eraserhead.clara-eql-test [r])
                      (r/insert (eav/->EAV 10 :foo/uuid "aaa"))
                      (r/fire-rules))]
      (count (r/query session query-results)) => 1)))
