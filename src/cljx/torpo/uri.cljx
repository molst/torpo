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

(defn parse "Parses 'uri-str'. 'base-uri' can optionally be used to give contextual info to the parser about 'uri-str'. For example, if 'uri-str' doesn't have a scheme it can't be parsed without that information being added via 'base-uri'. 'base-uri' is on the same format as an uri returned by this parse function."
  [uri-str & [{b-scheme :scheme :as base-uri}]]
  (when uri-str
    (let [[scheme remainder0] (clojure.string/split uri-str #":" 2)
          no-scheme-part (or remainder0 scheme)
          scheme (if (and remainder0 (> (count scheme) 0)) scheme b-scheme)
          [no-frag-part fragment] (clojure.string/split no-scheme-part #"#")
          [path params] (when (seq no-frag-part) (clojure.string/split no-frag-part #"\?"))
          param-map (when params (apply hash-map (apply concat (for [ppair (clojure.string/split params #"&")
                                                                     :let [[k v] (clojure.string/split ppair #"=")]
                                                                     :when (and (seq k) (not (empty? ppair)))]
                                                                 [(keyword k) v]))))
          [nada p2] (when path (clojure.string/split path #"//")) ;;remove any double slash
          p3 (or p2 nada)
          [hostname-and-port & rest-path] (when p2 (clojure.string/split p3 #"/")) ;;rest path always considered relative to host
          [hostname port] (when p2 (clojure.string/split hostname-and-port #":"))
          rest-path (if p2 rest-path (when p3 (clojure.string/split p3 #"/")))
          rest-path (if (= [] rest-path) [""] rest-path)] ;;Special treatment of the case when path is just "/". Absolute path always represented by an initial empty string.
      (clojure.core/merge
       (if fragment {:fragment fragment} {})
       (when param-map {:params param-map})
       (when (seq hostname) {:hostname hostname})
       (when port {:port (pl/parse-int port)})
       (when rest-path {:path (vec rest-path)})
       (when scheme {:scheme scheme})))))

(defn normalize "Transforms uri into an as common form as possible."
  [{:keys [hostname] :as uri}]
  (if false #_(and (seq hostname) (not (re-find #"^www\." hostname)))
    (assoc uri :hostname (str "www." hostname))
    uri))
