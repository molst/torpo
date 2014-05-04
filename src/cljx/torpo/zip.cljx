(ns torpo.zip
  (:require [clojure.zip :as z]
            [torpo.platform :as pl]))

(defn loc-path "Returns the locations leading to 'loc' from the root of 'loc'."
  [loc] (reverse (take-while #(not (nil? %)) (iterate z/up loc))))

(defn root-loc "Returns the root location of 'loc'. If 'n' is supplied, returns the 'loc' 'n' positions from the root location on the path to 'loc'."
  [loc & [n]] (first (drop (or n 0) (loc-path loc))))

(defn child-locs "Returns a seq of child locations of 'loc'."
  [loc] (take-while #(not (nil? %)) (iterate z/right (z/down loc))))

(defn- in-path? "If 'target-loc' is in the path of 'loc', returns 'target-loc', otherwise nil. Uses function 'nodes-equal?' to determine equality of nodes. Note that it returns nil also if (nodes-equal? loc target-loc)."
  [loc target-loc nodes-equal?]
  (first (drop-while #(and % (not (nodes-equal? (z/node %) (z/node target-loc)))) (iterate z/up loc))))

(defn edit-children "Recursively edits all 'loc's children using function 'f', which takes a node."
  [loc f nodes-equal?]
  (-> (drop-while #(in-path? % loc nodes-equal?) (iterate (fn [next-loc] (-> next-loc #_(z/edit next-loc f) z/next)) (z/next loc)))
      first z/prev (in-path? loc nodes-equal?)))
