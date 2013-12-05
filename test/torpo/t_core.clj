(ns torpo.t-core
  (:use midje.sweet)
  (:require [torpo.core :as torpo]))

;;Load the forms below to run all tests in the project.
#_(require 'midje.repl)
#_(midje.repl/load-facts)

(fact "merge-and-seq-distinctly works with numbers"
  (torpo/merge-and-seq-distinctly {:a 1} {:a 2} {:a 2} {:b 1}) => {:a '(1 2) :b 1})

(fact "merge-and-seq-distinctly works with string and vector"
  (torpo/merge-and-seq-distinctly {:member "kalle@kanel.se"} {:tags ["adde" "bladde"]}) => {:member "kalle@kanel.se" :tags ["adde" "bladde"]})

(fact "distinct-into-by chooses the right seq in case of conflict" (torpo/distinct-into-by first [[:A 1] [:B 2]] [[:A 4] [:C 3]]) => [[:A 1] [:B 2] [:C 3]])
(fact "distinct-into-by chooses the right seq in case of conflict" (torpo/distinct-into-by first [[:animal "bison"]] [[:animal "snake"]]) => [[:animal "bison"]])
(fact "distinct-into-by yields second coll in case of first coll is nil" (torpo/distinct-into-by first nil [[:A 4] [:C 3]]) => [[:A 4] [:C 3]])
(fact "distinct-into-by yields first coll in case of second coll is nil" (torpo/distinct-into-by first [[:A 1] [:B 2]] nil) => [[:A 1] [:B 2]])

(fact "arrange-tail, simple case" (torpo/arrange-tail [:A :B :C :D] [:C :A]) => [:B :D :C :A])
(fact "arrange-tail returns sorted map if input is a map" (torpo/arrange-tail {:A 1 :B 2 :C 3 :D 4} [:C :A]) => [[:B 2] [:D 4] [:C 3] [:A 1]])
(fact "arrange-tail with nil tail-keys" (torpo/arrange-tail [:A :B :C :D] nil) => [:A :B :C :D])

(fact "combine-reduce simple case" (torpo/combine-reduce merge [(fn [x] {:A 2}) (fn [x] {:B 3})] {:A 1}) => {:A 2 :B 3})