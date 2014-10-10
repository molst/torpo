(ns torpo.zip
  (:require [clojure.zip :as z]))

(defn reversed-loc-path [loc] (take-while #(not (nil? %)) (iterate z/up loc)))

(defn depth [loc] (count (z/path loc)))

(defn loc-path "Returns the locations leading to 'loc' from the root of 'loc'."
  [loc] (reverse (reversed-loc-path loc)))

(defn seek "Traverses 'loc' using 'traversal-fn' until 'pred' is fulfilled. Returns the seq of locations from the 'loc' at which the 'pred' is fulfilled to the root."
  [loc pred traversal-fn] (drop-while #(and (not (nil? %)) (not (pred %))) (iterate traversal-fn loc)))

(defn root-loc "Returns the root location of 'loc'. If 'n' is supplied, returns the 'loc' 'n' positions from the root location on the path to 'loc'."
  [loc & [n]] (first (drop (or n 0) (loc-path loc))))

(defn child-locs "Returns a seq of child locations of 'loc'."
  [loc] (take-while #(not (nil? %)) (iterate z/right (z/down loc))))

(defn in-path? "If 'target-loc' is in the path of 'loc', returns the loc in the 'loc' tree that equals 'target-loc', otherwise nil. Uses function 'nodes-equal?' to determine equality of nodes. Note that it returns nil also if (nodes-equal? loc target-loc)."
  [loc target-loc nodes-equal?] (first (drop-while #(and % (not (nodes-equal? (z/node %) (z/node target-loc)))) (iterate z/up loc))))

(defn delta-path "Returns the path between 'start-loc' and 'end-loc', including both, if they are found by stepping using 'traversal-fn'."
  [start-loc end-loc traversal-fn] (take-while #(and % (not= (z/node %) (z/node end-loc))) (iterate traversal-fn start-loc)))

(defn distance "Returns the number of steps using 'traversal-fn' needed in order to reach 'end-loc' if starting at 'start-loc'."
  [start-loc end-loc traversal-fn] (count (delta-path start-loc end-loc traversal-fn)))

(defn child-locs-r "Returns a seq of child locs recursively retrieved from 'loc'."
  [loc nodes-equal?] (take-while #(and (not (z/end? %)) (in-path? % loc nodes-equal?)) (iterate z/next (z/next loc))))

(defn edit-children-r "Recursively edits all 'loc's children using function 'f', which takes the node at the current child loc."
  [loc f nodes-equal?] (-> (take-while #(and % (in-path? % loc nodes-equal?))
                                       (iterate (fn [next-loc]
                                                  (let [nxt (z/next next-loc)]
                                                    (when (not (z/end? nxt)) (z/edit nxt f)))) loc)) last (in-path? loc nodes-equal?)))

(defn merge-into "Merges 'new-node' into 'loc' and returns a new 'loc', which is part of the updated zipper structure."
  [loc new-node] (z/replace loc (merge (z/node loc) new-node)))
