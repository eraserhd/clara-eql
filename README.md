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
  [clara-eql/QueryResult (= root 42) (= query 'test-query) (= data ?data)]
  =>
  (println "Data is now: " (pr-str ?data)))
```

## Assumptions

### All data is stored in EAV triples, in [clara_eav.eav.EAV] records

[clara_eav.eav.EAV]: https://cljdoc.org/d/clyfe/clara-eav/0.1.6/api/clara-eav.eav#EAV

### Attribute values in the EAV triples are keywords

Well, they don't have to be keywords, specifically, but this library does not
handle Datomic-style indirection where a number represents an attribute and a
keyword is assigned with a `:db/ident` property.

### Many-valued Attributes

The shape of the pull result differs for many-valued attributes.  `clara-eql`
assumes these attributes have a fact
`(->EAV attr-name :db/cardinality :db.cardinality/many)`.

### Identity Attributes

Identity attributes can be used for entity references, such as
`[:person/uuid #uuid "3fec670d-da20-4171-a96d-e5852be05ded"]`.  `clara-eql`
assumes identity attributes have a fact
`(->EAV attr-name :db/unique :db.unique/identity)`.

## License

Copyright © 2019 Jason Felice

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
