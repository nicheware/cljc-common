(ns nicheware.platform.utilities.common.config
  "Supports reading configuration from a file, merging environment (eg dev, test, prod) specific values with common configuration.
```clojure
{:env test :value 2}
```"
  (:require [promesa.core :as p]
            [kvlt.core :as kvlt]
            [cljstache.core :as tache]
            [nicheware.platform.utilities.common.core :as common]
            [clojure.string :as str]))

;; =========================== Constants ========================================

(def base-opts
  {:env-file-name "env-config.edn"
   :common-file-name "config.edn"
   :resolve-vars true})

(def re-mustache-match #"\{\{[^{}]*\}\}")

(defn make-render-fn
  "Execute the given function on the string used in a render, rendering the string before hand"
  [str-fn]
  (fn [text]
    (fn [render-fn]
      (str-fn (render-fn text)))))

(def render-fns
  {:upper (make-render-fn str/upper-case)
   :lower (make-render-fn str/lower-case)
   :capitalize (make-render-fn str/capitalize)
   })

;; ========================== Helper methods =====================================

(defn render
  "Render the given string using the provided map as value to be substituted for mustach style variables.
   Include a list of standard functions as part of the map. These include: upper, capatilize, lower

   use functions as {{#upper}}My-{{env}}{{/upper}}}}"
  [str val-map]
  (tache/render str (merge render-fns val-map)))

(defn resolve-template-vars
  "Resolves any template variables in a given map.
   Transforms to a string then feeds the map as the parameter resolver.
    Will recurse until no unresolved mustache variables."
  [val-map]
  (println "resolve-template-vars(), val-map: " val-map)
  (loop [vmap val-map]
    (let [map-str (pr-str vmap)
          resolved-str (render map-str vmap)
          resolved-map (common/edn-read resolved-str)
          match-mustache (re-find re-mustache-match resolved-str)]
      (if (nil? match-mustache)
        resolved-map
        (recur resolved-map)))))

(defn build-config-options
  "Builds config option from the given options passed in"
  [config-path opts]
  (let [ {:keys [env-file-name common-file-name] :as initial-opts} (merge base-opts opts)
        path-opts {:env-path (str config-path "/" env-file-name)
                   :common-path (str config-path "/" common-file-name)}]
    (merge path-opts initial-opts)))

;; ============================ Classpath config ================================

#?(:clj
   (defn read-config
     "Reads a single config file from the classpath, returning as a map."
     [config-file-path]
     (println "read-config(), config-file-path: " config-file-path)
     (if-let [config (some-> config-file-path
                             clojure.java.io/resource
                             slurp
                             clojure.edn/read-string)]
       config
       {})))

#?(:clj
   (defn load-config
     "Loads config from the classpath, using the given prefix for env and common files.

      - config-path:   path prefix to both env and common file (unless opts specifies paths)
      - opts:          optional ```{:env-file-name :env-path :common-file-name :common-path :resolve-vars}```
      - Returns: a map between merged env and common config."
     ([config-path] (load-config config-path {}))

     ([config-path opts]
      (let [{:keys [env-path common-path resolve-vars]} (build-config-options config-path opts)]
        (cond->        (read-config common-path)
          true         (common/deep-merge (read-config env-path))
          resolve-vars resolve-template-vars)))))


;; ========================== http config ========================================

(defn read-http-config
  "Reads via http from the default servers location (usually resources/public) the
   specified config-file at the given path.

   config-path:  eg 'nicheware/app'
   config-file:  eg 'config.edn'

  returns: A promise that resolves to the map of config values read."

  [config-url]
  (println "read-http-config(), config-url: " config-url)
  (-> (kvlt/request! {:url config-url :as :edn})
      (p/then :body)))


(defn load-http-config
  "Loads config from server, merging the current environment config with the base application configuration.
   Returns: A promesa promise that resolves to the merged config map"
  ([config-path] (load-http-config config-path {}))
  ([config-path opts]
   (let [{:keys [env-path common-path resolve-vars]} (build-config-options config-path opts)]

     ;; Read both config in parallel, waiting until both done
     (-> (p/all [(read-http-config env-path)
                 (read-http-config common-path)])

         ;; Merge the configs and optional resolve vars
         (p/then (fn [[env-config common-config]]
                   (println "load-http-config(): all complete, env-config: " env-config " common-config: " common-config)
                   (cond-> common-config
                     true (common/deep-merge env-config)
                     resolve-vars resolve-template-vars)))))))
