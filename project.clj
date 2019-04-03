(defproject net.eraserhead/clara-eql "0.1.3-SNAPSHOT"
  :description "Generate Clara rules to collect data from EDN Query Language queries."
  :url "https://github.com/eraserhd/clara-eql.git"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :username :env/clojars_username
                                    :password :env/clojars_password}]]
  :release-tasks [["vcs" "assert-committed"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["vcs" "commit"]
                  ["vcs" "tag" "v"]
                  ["deploy" "clojars"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["vcs" "commit"]
                  ["vcs" "push"]]
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [clyfe/clara-eav "0.1.6"]
                 [com.cerner/clara-rules "0.19.0"]
                 [edn-query-language/eql "0.0.3"]]
  :profiles {:dev
             {:dependencies [[midje "1.9.6"]
                             [zprint "0.4.15"]]
              :plugins      [[lein-midje "3.2.1"]]}}
  :repl-options {:init-ns dev})
