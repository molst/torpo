(ns torpo.zip
  (:require [clojure.zip :as z]))

(defn loc-path "Returns the locations leading to 'loc' from the root of 'loc'."
  [loc] (reverse (take-while #(not (nil? %)) (iterate z/up loc))))

(defn root-loc "Returns the root location of 'loc'. If 'n' is supplied, returns the 'loc' 'n' positions from the root location on the path to 'loc'."
  [loc & [n]] (first (drop (or n 0) (loc-path loc))))

(defn child-locs "Returns a seq of child locations of 'loc'."
  [loc] (take-while #(not (nil? %)) (iterate z/right (z/down loc))))

;;not used
#_(defn remove-children "Returns a new 'loc' with all children removed."
  [loc] (let [clocs (child-locs loc)
              first-child (first clocs)]
          (if first-child
            (first (take (count clocs) (iterate z/remove first-child)))
            loc)))

;;not used
#_(defn append-children "Returns a new 'loc' with 'child-items' appended."
  [loc child-items] (reduce (fn [parent item] (z/append-child parent item)) loc child-items))

;;not used
#_(defn reset-children "Returns a new 'loc' with reset children."
  [loc child-items] (-> (remove-children loc) (append-children child-items)))
