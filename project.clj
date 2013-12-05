(defproject torpo "0.1-SNAPSHOT"
  :description "A minimalistic helper library."
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :profiles {:dev {:dependencies [[midje "1.5.0"]]}}
  :scm {:name "git" :url "https://github.com/molst/torpo"})