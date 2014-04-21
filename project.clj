(defproject torpo "0.2-SNAPSHOT"
  :description "A minimalistic helper library."
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :url "https://github.com/molst/torpo"
  :scm {:name "git" :url "https://github.com/molst/torpo"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :profiles {:dev {:dependencies [[midje "1.6.3"]]}}
  :source-paths ["src/clj"]
  :plugins [[com.keminglabs/cljx "0.3.2"]]
  :cljx {:builds [{:source-paths ["src/cljx"]
                 :output-path "target/classes"
                 :rules :clj}
                {:source-paths ["src/cljx"]
                 :output-path "target/classes"
                 :rules :cljs}]}
  :hooks [cljx.hooks])