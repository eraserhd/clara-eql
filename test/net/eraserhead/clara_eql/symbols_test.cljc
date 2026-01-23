(ns net.eraserhead.clara-eql.symbols-test
  (:require
   [clojure.test :refer [deftest are]]
   [net.eraserhead.clara-eql.symbols :refer [key->variable]]))

(deftest t-key->variable
  (are [in out] (= out (key->variable in))
    :foo                     #_=> '?foo
    :foo/bar                 #_=> '?foo_SLASH_bar
    :foo.bar.baz/quux.blergh #_=> '?foo_DOT_bar_DOT_baz_SLASH_quux_DOT_blergh
    :quux.blergh             #_=> '?quux_DOT_blergh
    :foo_bar                 #_=> '?foo___bar))
