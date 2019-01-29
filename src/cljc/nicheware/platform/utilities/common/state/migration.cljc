(ns nicheware.platform.utilities.common.state.migration
"
  Functions used when handling migration of state, where state is represented by a Clojure map.

  The functions within the migration namespace can be categorised as:

|Function group|Functions|
|---|---|
|helper functions| [[major-version]], [[minor-version]], [[major-minor-version]], [[could-upgrade-version]]|
|migration interface| [[migrate-state]]|

  The migration functions assume the state map contains a version attribute

```clojure
  { :version \"1.0.1\"}
```

 Some functions require a migration map which for each current version defines the migrations that are defined
 to a later version.

```clojure
  {\"1.0\"{:from-version \"1.0\"
           :to-version \"1.1.0\"
           :migration-fns [identity]}

   \"1.1\"{:from-version \"1.1\"
           :to-version \"1.2.0\"
           :migration-fns [add-effects/add-pattern-effects]}}
```

The ```:migration-fns``` attribute is a vector of functions that accept the old state map and return a newly migrated state map.



"
  (:require [nicheware.platform.utilities.common.core :as common]
            [clojure.string :as str]))

;; ================================= Implementation helper functions ==================

(defn major-version
  "Extracts the major version as an int from a version string of the form 'x.x.x'

  - version: version number string of the form 'x.x.x' or 'x.x'
  - returns: The major version (first x) as an int. nil if not a valid version number.

"
  [version]
  (common/parse-int (common/before version ".")))

(defn major-minor-version
  "Returns the major minor version string, removing any revision number (if present)

  eg:
```clojure
(major-minor-version \"2.3.1\") => \"2.3\"
(major-minor-version \"1.1\") => \"1.1\"
```
"
  [version]
  (if (<  (count (str/split version #"\.")) 3)
    version
    (common/before-last version ".")))

(defn minor-version
  "Extracts the minor version as an int from a version string of the form 'x.x.x'

  - version: version number string of the form 'x.x.x' or 'x.x'
  - returns: The minor version (second x) as an int. nil if not a valid version number.

"
  [version]
  (common/parse-int (common/after (major-minor-version  version) ".")))

(defn could-upgrade-version
  "Test whether the first version is lower or equal to the second version. Just considers the major and minor versions,
   i.e. it is possible to upgrade.

   The revision number (last digit) is not considered, as it is deemed not necessary to upgrade for revision changes only.

   - first: Version number of form'x.x.x'
   - second: Version number of form'x.x.x'
   - returns: True if possible to upgrade from first to second (i.e. first is less than or equal to second)

"
  [first second]
  (let [first-major (major-version first)
        first-minor (minor-version first)
        second-major (major-version second)
        second-minor (minor-version second)]
    (or (< first-major second-major)
        (and (= first-major second-major)
             (<= first-minor second-minor)))))

;; ================================= Interface functions ===============================

(defn migrate-state
  "Migrate state from its existing version to the goal version using the
  supplied migration functions.

  - incoming-state: Current state including the version number. ```{:version \"1.2\", ...}```
  - target-version: New version state should migrate to. eg ```\"1.3.1\"```
  - migrations: Map defining migrations for different source and target versions. See below.
  - returns: a new state map, upgraded to the target-version.

migrations:

```clojure
{<source-migration-version> {:from-version :to-version :migration-fns[]]}}
```

  ```:migration-fns``` is a vector of functions that when applied in order will accept a state map conforming to the ```:from-version``` state
  schema, and will return a new state map conforming to the ```:to-version``` schema.

  migrate-state will work out which migrations need to be applied to convert the state to the eventual target version.

"
  [{:keys [version] :as incoming-state} target-version migrations]

  (loop [current version
         state incoming-state]
    (println "migrate-state(): current: " current " state.version: " (:version state) " target: " target-version)
    (if-let [migration (get migrations (major-minor-version current))]

      ;; Migration exists from current state, migrate if resulting state compatible with target
      (let [{:keys [to-version migration-fns]} migration]
        (if (could-upgrade-version to-version target-version)
          (recur to-version
                 (-> state
                     ((common/compose-fns migration-fns))
                     (assoc :version to-version)))
          state))

      ;; No migration from the current state, so just return it.
      state)))
