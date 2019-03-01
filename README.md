# clara-eql

Generate Clara rules to collect data from EDN Query Language queries.
[![CircleCI](https://circleci.com/gh/eraserhd/clara-eql.svg?style=svg)](https://circleci.com/gh/eraserhd/clara-eql)
## Usage

```clojure
(require '[net.eraserhead.clara-eql :as clara-eql])

(clara-eql/defrule test-query
  :query
  [:person/first-name
   :person/last-name
   {:person/supervisor
    [:person/uuid]}]
  :from ?eid
  :where
  [EAV (= e ?eid) (= a :person/uuid)]
  [:test (even? ?eid)])

(defrule foo
  [clara-eql/QueryData (= root 42) (= query 'test-query) (= data ?data)]
  =>
  (println "Data is now: " (pr-str ?data)))
```

## License

Copyright © 2019 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
