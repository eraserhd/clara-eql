(ns build
  (:require
   [clojure.tools.build.api :as b]
   [deps-deploy.deps-deploy :as d]))

(def lib 'net.eraserhead/clara-eql)
(def version "0.2.0")
(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(def basis (delay (b/create-basis {:project "deps.edn"})))

(def rev (b/git-process {:git-args "rev-parse HEAD"}))

(def pom-options
  {:class-dir class-dir
   :lib       lib
   :version   version
   :basis     @basis
   :src-dirs  ["src"]
   :pom-data  [[:description "Generate Clara rules to collect data from EDN Query Language queries."]
               [:url "https://github.com/eraserhd/clara-eql.git"]
               [:scm
                [:url "https://github.com/eraserhd/clara-eql"]
                [:connection "scm:git:git://github.com/eraserhd/clara-eql.git"]
                [:developerConnection "scm:git:ssh://git@github.com/eraserhd/clara-eql.git"]
                [:tag rev]]
               [:licenses
                [:license
                 [:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"]
                 [:url "https://opensource.org/license/epl-2-0"]]]]})

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (b/write-pom pom-options)
  (b/copy-dir  {:src-dirs  ["src" "resources"]
                :target-dir class-dir})
  (b/jar       {:class-dir class-dir
                :jar-file  jar-file}))

(defn release
  "Publish to Clojars."
  [_]
  (clean nil)
  (jar nil)
  (d/deploy {:installer      :remote
             :sign-releases? true
             :artifact       jar-file
             :pom-file       (b/pom-path pom-options)}))
