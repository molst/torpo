(ns torpo.core
  (:require [clojure.zip :as zip]))

(defn first-if-seq "Yields first item in 'o' if 'o' is sequential, otherwise 'o' itself."
  [o] (if (sequential? o) (first o) o))

(defn xor [x y] (or (and x (not y))
                    (and y (not x))))

(defn insert-distinctly-for-key
  "Inserts new-coll into old-coll. All items for which the value at distinct-key-path exist in both new-coll and old-coll, the item in the new-coll is chosen."
  [new-coll old-coll distinct-key-path]
  (into new-coll
        (for [old-item old-coll :when (not (some #(= (get-in % distinct-key-path :not-found-1) (get-in old-item distinct-key-path :not-found-2)) new-coll))]
          old-item)))

(defn distinct-into-by "Returns a seq containing all items in 'to', and all items in 'from' for which 'f' does not return equal values in both 'to' and 'from'."
  [f to from]
  (concat to
   (filter (fn [from-item] ;;remove all items in 'from' that also exists in 'to'
             (not (some
               (fn [to-item]
                 (= (f to-item) (f from-item)))
               to)))
           from)))

(defn paths-to-key-value
  "Goes through a map/seq tree on the form {:target-key \"value\" :tree-key ({:target-key \"value\" :tree-key ()})} to find all occurrences to the supplied target-key/value combination. Returns the path of values to get there."
  [target-key value tree-key map-arg]
  (loop [loc (zip/zipper #(and (map? %) (target-key %)) #(tree-key %) (fn [node children] {tree-key children}) map-arg)
         paths ()]
    (if (zip/end? loc)
      paths
      (if (and (zip/node loc) (re-matches (re-pattern value) (target-key (zip/node loc))))
        (recur
          (zip/next loc)
          (conj paths (conj (zip/path loc) (zip/node loc)))) ;;include the target node, not only the path to get to the target node
        (recur
          (zip/next loc)
          paths)))))

;;see "call-on-each-node" in util.cljs
(defn remap-keys "Remaps the keys of 'map' that has corresponding keys in 'old-to-new-map' to the values of 'old-to-new-map'."
  [map old-to-new-map]
  (loop [original-keys (keys map)
         lmap map]
    (if (empty? original-keys)
      lmap
      (recur
       (rest original-keys)
       (if-let [new-key ((first original-keys) old-to-new-map)]
         (assoc (dissoc map (first original-keys)) new-key ((first original-keys) map)))))))

(defn truncate [s max-result-length]
  (if (> (count s) max-result-length)
    (str (subs s 0 (- max-result-length 3)) "...")
    s))

(defn full-name [obj]
  (assert (keyword? obj) (str "Function 'full-name' does not support type '" (type obj) "' yet."))
  (let [n-name (when-let [n-name (namespace obj)] (str n-name "/"))]
    (when-let [name (name obj)]
      (str n-name name))))

(defn merge-and-seq-distinctly "Example: the arguments {:a 1} {:a 2} {:a 2} {:b 1} are converted to {:a (1 2) :b 1}."
  [map & maps]
  (let [maps (conj maps map)
        all-key-vals (apply concat (map seq maps))
        grouped-by-key (group-by first all-key-vals)]
    (apply merge
           (for [entry (seq grouped-by-key)
                 :let [val (val entry)
                       val (if (> (count val) 1) (distinct (for [seq-val val] (second seq-val)))
                               (second (first val)))]]
             {(key entry) val}))))

(defn sub-map? "Checks if 'target-map' has all the keys and values of 'template-map'." [target-map template-map]
  (= template-map (select-keys target-map (keys template-map))))

(defn sub-maps "Gets the maps in coll that template-map is a submap of." [coll template-map]
  (filter #(sub-map? % template-map) coll))

;;duplicated between clj and cljs
(defn doto-vals "Applies 'f' to each value of the supplied map." [f map]
  (apply merge (for [[k v] (seq map) :let [fv (when v (f v))] :when fv] {k (f v)})))

(defn arrange-tail "Sorts 'coll' so that items that can be identified by 'tail-keys' are put last and in the order of 'tail-keys'. In case 'coll' is associative, returns a seq of key value pairs."
  [coll tail-keys]
  (if (seq tail-keys)
    (let [c (seq coll)
         {in-tail-keys true not-in-tail-keys false} (group-by (fn [c-item] (true? (some #(= (first-if-seq c-item) %) tail-keys))) c)
         sorted-in-tail-keys (sort-by #(.indexOf tail-keys (first-if-seq %)) in-tail-keys)
         result (concat not-in-tail-keys sorted-in-tail-keys)]
     result)
    coll))

(defn arrange-map-tail "Creates a map that is arranged so that the keys matching 'tail-keys' are put last and in the order of 'tail-keys'."
  [map tail-keys]
  (->> (arrange-tail map tail-keys) (apply concat) (apply sorted-map)))

(defn seqify-entries "Takes any collection and turns it into a seq of seqs. If the 'coll' items are no seqs already and cannot naturally become that, the seqs will be on the form [entry nil]. Eg. (seqify-entries [:A :B [1 2 3]]) yields [[:A nil] [:B nil] [1 2 3], (seqify {:A 1 :B 2}) yields [[:A 1] [:B 2]]."
  [coll]
  (map (fn [x] (if (sequential? x) x [x nil])) (seq coll)))

(defmacro merge-result "If 'val' is a map and the result of evaluating 'forms' is a map, the result will be merged into 'val'."
  [val & forms]
  `(let [result# (do ~@forms)]
     (if (and (map? ~val) (map? result#)) (merge ~val result#) result#)))

(defn merge-in "Merges 'val' into 'm'."
  [m location val] (update-in m location #(merge-result % val)))

(defn merge-reduce "Runs each function in 'functions' and merges the result into a resulting map. Each function takes the accumulated merged result map as the first argument, and, if present, additional arguments according to 'opargs', and returns the resulting map."
  [functions init-map & opargs]
  (reduce (fn [acc f] (merge acc (when opargs (apply (partial f acc) opargs) (f acc)))) init-map (distinct functions)))

(defn reduce-calls "Calls each function in 'functions'. Each function takes the accumulated combined result as the first argument, and, if present, additional arguments according to 'opargs', and returns the resulting object."
  [functions init-obj & opargs]
  (reduce (fn [acc f] (apply (partial f acc) opargs)) init-obj (distinct functions)))
