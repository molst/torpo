(ns torpo.uri
  (:refer-clojure :exclude [merge])
  #+clj (:import (java.net URLEncoder URLDecoder))
  #+cljs (:require-macros [clojure.core :refer [some->]])
  (:require [torpo.core :as core]
            [torpo.platform :as pl]))

(defn make-uri-params "Makes a uri-params string out of a map. Keys that are keywords will get the colon removed before converting it to a string."
  [params-map]
  (clojure.string/join "&" (map #(str (core/full-name (first %)) "=" (second %)) (filter second (seq params-map)))))

(defn make-uri-path "Takes a seq of uri-section strings and makes a uri path string. If the last section is a map instead of a string, it will be treated as query key-value pairs to be stringified and appended."
  [& uri-sections]
  (when (seq uri-sections)
    (let [last-section (last uri-sections)
          param-map (if (map? last-section) last-section nil)
          uri-param-str (make-uri-params param-map)
          uri-sections (filter #(and (not (nil? %)) (not (map? %))) uri-sections)] ;;remove nils and maps
      (str "/" (clojure.string/join "/" uri-sections) (when (seq uri-param-str) (str "?" uri-param-str))))))

(defn make-uri-str [{:keys [scheme hostname port path params fragment]}]
  (str (if scheme (str scheme ":" (if (or (= scheme "http") (= scheme "https")) "//")) "")
       (or hostname "")
       (if port (str ":" port) "")
       (when path (clojure.string/join "/" (if (and hostname (not (empty? (first path)))) (cons "" path) path))) ;;the 'if' ensures there is always a '/' between a host and a path if both exist
       (if params (str "?" (clojure.string/join "&" (map #(str (name (key %)) "=" (val %)) (seq params)))) "")
       (if fragment (str "#" fragment) "")))

(defn get-last-path-component [uri-string]
  (when uri-string (last (clojure.string/split uri-string #"/"))))

(defn replace-last-path-component [uri-string last-component]
  (when uri-string (clojure.string/join "/" (concat (butlast (clojure.string/split uri-string #"/")) `(~last-component)))))

(defn to-file-path [file-uri-str]
  (when (seq file-uri-str)
    (-> (clojure.string/replace-first file-uri-str "file:///" "/")
        (clojure.string/replace-first "file://localhost/" "/"))))

(defn decode
  [string]
  (some-> string str
          #+clj (URLDecoder/decode "UTF-8")
          #+cljs (js/decodeURIComponent)))

(defn prepare-for-transmission "Does things to 'uri' that should only be done once before it is transmitted."
  [uri] (update-in uri [:params] (fn [old-val] (core/doto-vals pr-str (:params uri)))))

(defn port [uri] (or (:port uri) (case (:scheme uri) "http" 80 "https" 443)))

(defn merge "Merges all supplied uri's from left to right."
  [u1 u2 & ux]
  (let [all-uris (concat [u1 u2] ux)]
    (reduce (fn [merged-uri u]
              (clojure.core/merge (clojure.core/merge merged-uri u) ;start with a basic overwriting merge at the top map level...
                                  (let [path (vec (concat (:path merged-uri) (:path u)))] (when (seq path) {:path path})) ;... then do the special uri-specific merge stuff for some of the keys
                                  (when-let [params (clojure.core/merge (:params merged-uri) (:params u))] {:params params})))
            all-uris)))

(defn parse [uri-str]
  (when uri-str
    (let [[remainder1 fragment] (clojure.string/split uri-str #"#")
          [scheme remainder2] (when remainder1 (clojure.string/split remainder1 #":" 2))
          remainder3 (or remainder2 scheme) ;;if no remainder is found the only way this could be a valid uri is if the source str (now "scheme") represents a "protocol-relative" uri
          [path params] (when remainder3 (clojure.string/split remainder3 #"\?")) ;;the first section of path can be a hostname in case of "http" or "https"
          param-map (when params (apply hash-map (apply concat (for [ppair (clojure.string/split params #"&")
                                                                     :let [[k v] (clojure.string/split ppair #"=")]
                                                                     :when (and (seq k) (not (empty? ppair)))]
                                                                 [(keyword k) v]))))]
      (clojure.core/merge
       (if (and scheme remainder2) ;;when we've got an explicit scheme...
         (clojure.core/merge {:scheme scheme}
                             (when (or (= scheme "http") (= scheme "https"))
                               (let [[nada p2] (clojure.string/split path #"//") ;;just remove the double slash
                                     [hostname-and-port & rest-path] (clojure.string/split p2 #"/") ;;rest path always considered relative to host
                                     [hostname port] (clojure.string/split hostname-and-port #":")]
                                 (clojure.core/merge {} (when (seq hostname) {:hostname hostname})
                                                     (when port {:port (pl/parse-int port)})
                                                     (when rest-path {:path (vec rest-path)})))))
         ;;if the first section in path is an empty string, that indicates a root path
         (clojure.core/merge {} (when (seq path) (when-let [path-seq (seq (clojure.string/split path #"/"))] {:path (vec path-seq)}))))
       (when param-map {:params param-map}) ;;query/params are usually only part of http and https requests, but might be used also in other circumnstances.
       (when fragment  {:fragment fragment}))))) ;;Fragment valid depending on parent document mime type. Should not participate in client/server communication.

(defn normalize "Transforms uri into an as common form as possible."
  [{:keys [hostname] :as uri}]
  (if (and (seq hostname) (not (re-find #"^www\." hostname)))
    (assoc uri :hostname (str "www." hostname))
    uri))
