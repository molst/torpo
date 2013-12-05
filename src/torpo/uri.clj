(ns torpo.uri
  (:require [torpo.core :as core]))

;;Note that make-uri-params is duplicated between the cljs and clj code bases
(defn make-uri-params "Makes a uri-params string out of a map. Keys that are keywords will get the colon removed before converting it to a string."
  [params-map]
  (clojure.string/join "&" (map #(str (core/full-name (first %)) "=" (second %)) (filter second (seq params-map)))))

;;Note that make-uri-path is duplicated between the cljs and clj code bases
(defn make-uri-path "Takes a seq of uri-section strings and makes a uri path string. If the last section is a map instead of a string, it will be treated as query key-value pairs to be stringified and appended."
  [& uri-sections]
  (when (seq uri-sections)
    (let [last-section (last uri-sections)
          param-map (if (map? last-section) last-section nil)
          uri-param-str (make-uri-params param-map)
          uri-sections (filter #(and (not (nil? %)) (not (map? %))) uri-sections)] ;;remove nils and maps
      (str "/" (clojure.string/join "/" uri-sections) (when (seq uri-param-str) (str "?" uri-param-str))))))

(defn make-uri-str [{:keys [scheme hostname port path params]}]
  (str scheme "://" hostname ":" port (apply make-uri-path (if [(seq params)] (conj path params) path))))

(defn get-last-path-component [uri-string]
  (when uri-string (last (clojure.string/split uri-string #"/"))))

(defn replace-last-path-component [uri-string last-component]
  (when uri-string (clojure.string/join "/" (concat (butlast (clojure.string/split uri-string #"/")) `(~last-component)))))

(defn to-file-path [file-uri-str]
  (when (seq file-uri-str)
    (-> (clojure.string/replace-first file-uri-str "file:///" "/")
        (clojure.string/replace-first "file://localhost/" "/"))))

;;Duplicated between clj and cljs
(defn prepare-for-transmission "Does things to 'uri' that should only be done once before it is transmitted."
  [uri] (update-in uri [:params] (fn [old-val] (core/doto-vals pr-str (:params uri)))))

(defn merge "Merges all supplied uri's from left to right."
  [u1 u2 & ux]
  (let [all-uris (concat [u1 u2] ux)
        defaults {:request-method :get}]
    (reduce (fn [merged-uri u]
              (assoc (clojure.core/merge merged-uri u) ;start with a basic overwriting merge at the top map level...
                :path (vec (concat (:path merged-uri) (:path u))) ;... then do the special uri-specific merge stuff for some of the keys
                :params (clojure.core/merge (:params merged-uri) (:params u))))
            defaults all-uris)))