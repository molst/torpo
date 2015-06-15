(defproject torpo "0.5.0"
  :description "A minimalistic helper library."
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :url "https://github.com/molst/torpo"
  :scm {:name "git" :url "https://github.com/molst/torpo"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :profiles {:dev {:dependencies [[midje "1.6.3"]]}}
  :source-paths ["src/clj"]
  :plugins [[com.keminglabs/cljx "0.5.0"]]
  :cljx {:builds [{:source-paths ["src/cljx"]
                 :output-path "target/classes"
                 :rules :clj}
                {:source-paths ["src/cljx"]
                 :output-path "target/classes"
                 :rules :cljs}]}
  :prep-tasks [["cljx" "once"] "javac" "compile"])
