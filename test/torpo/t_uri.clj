(ns torpo.t-uri
  (:use midje.sweet)
  (:require [torpo.uri :as uri]))

;;Load the forms below to run all tests in the project.
#_(require 'midje.repl)
#_(midje.repl/load-facts)

(def simple-http-uri "http://hoist/A")
(def simple-path-with-frag "/A#frag")
(def fat-uri  "http://hoist:44/A/B?p1=v1&p2=v2#frag")
(def fat-uri2 "http://hoisu:66/C/D?q1=v1&q2=v2#fraggel")
(def fat-uri-relative "C/D?q1=v1&q2=v2#fraggel")
(def one-param "http://hoist:44/A?p1=v1#frag")
(def empty-frag "#")
(def only-frag "#frag")
(def only-root "/")
(def simple-absolut-path "/A")
(def simple-relative-path "A")
(def simple-relative-path-with-slash "A")

(fact "parse simple http uri" (uri/parse simple-http-uri) => {:hostname "hoist" :path ["A"] :scheme "http"})
(fact "parse fragment from simple path" (uri/parse simple-path-with-frag) => {:path ["" "A"] :fragment "frag"})
(fact "fat uri" (uri/parse fat-uri) => {:hostname "hoist" :path ["A" "B"] :port 44 :scheme "http" :params {:p1 "v1" :p2 "v2"} :fragment "frag"})
(fact "empty frag" (uri/parse empty-frag) => {})
(fact "only frag" (uri/parse only-frag) => {:fragment "frag"})
(fact "only root" (uri/parse only-root) => {:path [""]})
(fact "parse simple absolute path" (uri/parse simple-absolut-path) => {:path ["" "A"]})
(fact "parse simple relative path" (uri/parse simple-relative-path) => {:path ["A"]})
(fact "parse simple relative path with slash" (uri/parse simple-relative-path-with-slash) => {:path ["A"]})
(fact "get port" (uri/port (uri/parse fat-uri)) => 44)
(fact "get defualt http port" (uri/port (uri/parse simple-http-uri)) => 80)
(fact "parse javascript href" (uri/parse "javascript:void(0);") => {:scheme "javascript"}) ;;No support for parsing this scheme further yet
(fact "parse ?& param" (uri/parse "http://www.blocket.se/vasterbotten?&ca=2sp=1&w=1") => {:hostname "www.blocket.se", :params {:ca "2sp", :w "1"}, :path ["vasterbotten"], :scheme "http"})
(fact "parse strange params" (uri/parse "http://www.blocket.se/vasterbotten?&&a=&=3&w=4=5") => {:hostname "www.blocket.se", :params {:a nil, :w "4"}, :path ["vasterbotten"], :scheme "http"})
(fact "parse scheme-relative uri" (uri/parse "//www.blocket.se/vasterbotten") => {:hostname "www.blocket.se", :path ["vasterbotten"]})

(defn parse-uri-roundtrip [uri-str] (-> uri-str (uri/parse) (uri/make-uri-str)))
(fact "parse roundtrip simple http uri"      (parse-uri-roundtrip simple-http-uri) => simple-http-uri)
(fact "parse roundtrip one-param"            (parse-uri-roundtrip one-param) => one-param)
(fact "parse roundtrip simple aboslute path" (parse-uri-roundtrip simple-absolut-path) => simple-absolut-path)
(fact "parse roundtrip javascript"           (parse-uri-roundtrip "javascript:void(0);") => "javascript:") ;;No support for parsing this scheme further yet

(fact "merge" (uri/merge (uri/parse fat-uri) (uri/parse fat-uri2)) =>
      {:fragment "fraggel" :hostname "hoisu" :params {:p1 "v1" :p2 "v2" :q1 "v1" :q2 "v2"} :path ["A" "B" "C" "D"] :port 66 :scheme "http"})

(fact "resolve-relative" (uri/resolve-relative (uri/parse fat-uri) (uri/parse fat-uri-relative)) =>
      {:fragment "fraggel" :hostname "hoist" :params {:q1 "v1" :q2 "v2"} :path ["A" "C" "D"] :port 44 :scheme "http"})

(fact "simple file uri roundtrip" (parse-uri-roundtrip "file:///a/b") => "file:///a/b")
(fact "make uri of only scheme and path" (uri/make-uri-str {:scheme "http", :path ["a" "b"]}) => "http:///a/b")
(fact "make uri of relative path" (uri/make-uri-str {:path ["A"]}) => "A")
(fact "make uri of absolute path" (uri/make-uri-str {:path ["" "A"]}) => "/A")

(fact "prepare for transmission preserves Clojure code for parsing with read-string"
      (uri/prepare-for-transmission {:path ["p1" "p2"] :params {:a "1"}}) => {:params {:a "%221%22"}, :path ["p1" "p2"]})

(fact "make-uri-str after prepare-for-transmission"
      (uri/make-uri-str (uri/prepare-for-transmission {:path ["p1" "p2"] :params {:a "1"}})) => "p1/p2?a=%221%22")