(ns torpo.platform)

(defn parse-int [int-str] (#+clj Integer/parseInt #+cljs js/parseInt int-str))

(defn throw-str [s] (throw (#+clj Exception. #+cljs js/Error. s)))

(defn date [] #+clj (java.util.Date.) #+cljs (js/Date.))

(defn debuglog [message-str] #+clj (println message-str) #+cljs (.log js/console message-str))
