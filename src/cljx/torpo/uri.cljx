(ns torpo.uri
  (:refer-clojure :exclude [merge])
  #+clj (:import (java.net URLEncoder URLDecoder))
  #+cljs (:require-macros [clojure.core :refer [some->]])
  (:require [torpo.core :as core]
            [torpo.platform :as pl]
            [clojure.string :as string]))

(defn make-uri-params "Makes a uri-params string out of a map. Keys that are keywords will get the colon removed before converting it to a string."
  [params-map]
  (string/join "&" (map #(str (core/full-name (first %)) "=" (second %)) (filter second (seq params-map)))))

(defn make-uri-path "Takes a seq of uri-section strings and makes a uri path string. If the last section is a map instead of a string, it will be treated as query key-value pairs to be stringified and appended."
  [& uri-sections]
  (when (seq uri-sections)
    (let [last-section (last uri-sections)
          param-map (if (map? last-section) last-section nil)
          uri-param-str (make-uri-params param-map)
          uri-sections (filter #(and (not (nil? %)) (not (map? %))) uri-sections)] ;;remove nils and maps
      (str "/" (string/join "/" uri-sections) (when (seq uri-param-str) (str "?" uri-param-str))))))

(defn normalized-request-method [uri]
  (or (when-let [rm (:request-method uri)] (string/upper-case (name rm)))
      "GET"))

(defn make-path-str [path] (string/join "/" path))
(defn make-params-str [params] (string/join "&" (map (fn [[k v]] (str (name k) "=" (string/trim v))) (seq params))))
(defn make-path-params-str [path params] (str (make-path-str path) (when (seq params) (str "?" (make-params-str params)))))

(defn make-uri-str [{:keys [scheme hostname port path params fragment]}]
  (str (when scheme (str scheme "://"))
       (or hostname "")
       (if port (str ":" port) "")
       (when path (str (if scheme "/" "") (if (string? path) path (string/join "/" path))))
       ;;all params must be strings so they can be consistently read back with read-string if they contain, for example, Clojure data
       (if params (str "?" (string/join "&" (map (fn [[k v]] (str (name k) "=" (string/trim v))) (seq params)))) "")
       (if fragment (str "#" fragment) "")))

(defn get-last-path-component [uri-string]
  (when uri-string (last (string/split uri-string #"/"))))

(defn replace-last-path-component [uri-string last-component]
  (when uri-string (string/join "/" (concat (butlast (string/split uri-string #"/")) `(~last-component)))))

(defn to-file-path [file-uri-str]
  (when (seq file-uri-str)
    (-> (string/replace-first file-uri-str "file:///" "/")
        (string/replace-first "file://localhost/" "/"))))

(defn encode-str
  [string]
  (some-> string str
          #+clj (URLEncoder/encode "UTF-8")
          #+cljs (js/encodeURIComponent)
          (.replace "+" "%20")))

(defn decode-str
  [string]
  (some-> string str
          #+clj (URLDecoder/decode "UTF-8")
          #+cljs (js/decodeURIComponent)))

(defn prepare-for-transmission "Does things to 'uri' that should only be done once before it is transmitted."
  [uri] (update-in uri [:params] (fn [old-val] (core/doto-vals #(let [s (pr-str %)]
                                                                    (if (= :post (:request-method uri))
                                                                      s
                                                                      (encode-str s)))
                                                               (:params uri)))))

(defn port [uri] (or (:port uri) (case (:scheme uri) "http" 80 "https" 443)))

(defn merge "Merges all supplied uri's from left to right."
  [u1 u2 & ux]
  (let [all-uris (concat [u1 u2] ux)]
    (reduce (fn [merged-uri u]
              (clojure.core/merge (clojure.core/merge merged-uri u) ;start with a basic overwriting merge at the top map level...
                                  (let [path (vec (concat (:path merged-uri) (:path u)))] (when (seq path) {:path path})) ;... then do the special uri-specific merge stuff for some of the keys
                                  (when-let [params  (clojure.core/merge (:params  merged-uri) (:params  u))] {:params  params})
                                  (when-let [headers (clojure.core/merge (:headers merged-uri) (:headers u))] {:headers headers})))
            all-uris)))

(defn resolve-relative "Returns an absolute version of 'uri' resolved according to the official URI spec, based on the absolute 'base-uri'."
  [base-uri uri]
  (clojure.core/merge (clojure.core/merge base-uri uri)
                      (when-let [npath (:path uri)]
                        (let [opath (if-let [opath (:path base-uri)] (drop-last opath) [])]
                          {:path (vec (concat opath npath))}))))

(defn parse [uri-str]
  (when uri-str
    (let [[scheme remainder0] (string/split uri-str #":" 2)
          no-scheme-part (or remainder0 scheme)
          scheme (when remainder0 scheme)
          [no-frag-part fragment] (string/split no-scheme-part #"#")
          [path params] (when (seq no-frag-part) (string/split no-frag-part #"\?"))
          param-map (when params (apply hash-map (apply concat (for [ppair (string/split params #"&")
                                                                     :let [[k v] (string/split ppair #"=")]
                                                                     :when (and (seq k) (not (empty? ppair)))]
                                                                 [(keyword k) v]))))
          [nada p2] (when path (string/split path #"//")) ;;remove any double slash
          p3 (or p2 nada)
          [hostname-and-port & rest-path] (when p2 (string/split p3 #"/")) ;;rest path always considered relative to host
          [hostname port] (when p2 (string/split hostname-and-port #":"))
          rest-path (if p2 rest-path (when p3 (string/split p3 #"/")))
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
