(ns nicheware.platform.utilities.common.config
  "Supports reading application configuration as a clojure map.

   Features include:

   - merging of a global config file, with an environment specific one (eg dev, test, prod)
   - mustache variable substitution in config values. Variables are previous defined config values.
   - mustache function operation on config value.
   - optional parameter to override defaults.

   As an example the common configuration could be defined in ```resources/app/config.edn```

```clojure
{:env \"default\"
 :value \"common-value\"
 :file \"{{env}}-file.txt\"}
```

   and environment specific config defined in ```env/test/resources/app/env-config.edn```

```clojure
{:env \"test\"
 :other \"value\"}
```

   Calling ```(load-config \"app\")``` will return:

```clojure
{:env \"test\"
 :other \"value\"
 :value \"common-value\"
 :file \"test-file.txt\"}
```

   Variable evaluation is done recursively, on the final merged map until all variables have been evaluated.

   Both [[load-config]] and [[load-http-config]] accept an optional opts map, which can selectively override theme
   the following defaults

```clojure
{:env-file-name \"env-config.edn\"
 :common-file-name \"config.edn\"
  :resolve-vars true}
```

  The opts arguments may also define ```common-path``` or ```env-path```, which would then be used
  instead of building up the path from the file name and provided path prefix.
"
  (:require [promesa.core :as p]
            [kvlt.core :as kvlt]
            [cljstache.core :as tache]
            [nicheware.platform.utilities.common.core :as common]
            [clojure.string :as str]))

;; =========================== Constants ========================================

(def ^:no-doc base-opts
  {:env-file-name "env-config.edn"
   :common-file-name "config.edn"
   :resolve-vars true})

(def ^:no-doc re-mustache-match #"\{\{[^{}]*\}\}")

(defn ^:no-doc make-render-fn
  "Execute the given function on the string used in a render, rendering the string before hand"
  [str-fn]
  (fn [text]
    (fn [render-fn]
      (str-fn (render-fn text)))))

(def ^:no-doc render-fns
  {:upper (make-render-fn str/upper-case)
   :lower (make-render-fn str/lower-case)
   :capitalize (make-render-fn str/capitalize)
   })

;; ========================== Helper methods =====================================

(defn ^:no-doc render
  "Render the given string using the provided map as value to be substituted for mustach style variables.
   Include a list of standard functions as part of the map. These include: upper, capatilize, lower

   use functions as {{#upper}}My-{{env}}{{/upper}}}}"
  [str val-map]
  (tache/render str (merge render-fns val-map)))

(defn ^:no-doc resolve-template-vars
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

(defn ^:no-doc build-config-options
  "Builds config option from the given options passed in"
  [config-path opts]
  (let [ {:keys [env-file-name common-file-name] :as initial-opts} (merge base-opts opts)
        path-opts {:env-path (str config-path "/" env-file-name)
                   :common-path (str config-path "/" common-file-name)}]
    (merge path-opts initial-opts)))

;; ============================ Classpath config ================================

#?(:clj
   (defn read-config
     "Reads a single config file from the classpath, returning as a clojure map.

     - config-file-path:  eg ```app/config.edn```"
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

      - config-path:   path prefix to both env and common file
      - opts:          optional overrides of default values ```{:env-file-name :env-path :common-file-name :common-path :resolve-vars}```
      - Returns:       a map between merged env and common config."
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

   - config-url:  eg ```app/config.edn```
   - returns: a promesa promise that resolves to the map of config values read.
  "
  [config-url]
  (println "read-http-config(), config-url: " config-url)
  (-> (kvlt/request! {:url config-url :as :edn})
      (p/then :body)))


(defn load-http-config
  "Loads config from server, merging the current environment config with the base application configuration.

   As the server represents and environment, it is up to the deployment build process to ensure the appropriate env-config.edn
   is deployed, typically from ```env/<environment>/resources/public/<app-prefix>/env-config.edn```.

   - config-path: eg ```app``` (typically will look for ```/app/config.edn``` and ```/app/env-config.edn```)
   - opts:        optional of default values ```{:env-file-name :env-path :common-file-name :common-path :resolve-vars}```
   - returns:     A promesa promise that resolves to the merged config map"
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
