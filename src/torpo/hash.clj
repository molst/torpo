(ns torpo.hash)

(defn get-hash [type data]
  (.digest (java.security.MessageDigest/getInstance type) (.getBytes data)))

(defn get-hash-str [data-bytes]
  (when (seq data-bytes) (apply str (map #(.substring (Integer/toString (+ (bit-and % 0xff) 0x100) 16) 1) data-bytes))))

(defn sha1 [data]
  (when (seq data) (get-hash "sha1" data)))

(defn sha1-str [data]
  (get-hash-str (sha1 data)))

(defn make-unique-token [& seed]
  (sha1-str (str "qka0&0l3kdfabbqnbw82n--^" ;a string that is simply hard to guess if you don't have access to this source code
                 (reduce str (if (map? seed) (vals seed) seed))
                 (rand-int 0xffff)
                 (.getTime (java.util.Date.)))))
