(ns nicheware.platform.utilities.common.state.migration
  (:require [nicheware.platform.utilities.common.core :as common]
            [clojure.string :as str]))

;; ================================= Implementation helper functions ==================

(defn major-version
  "Extracts the major version as an int from a version string of the form 'x.x.x'"
  [version]
  (common/parse-int (common/before version ".")))

(defn major-minor-version
  "Returns the major minor version string, removing any path number (if present)"
  [version]
  (if (<  (count (str/split version #"\.")) 3)
    version
    (common/before-last version ".")))

(defn could-upgrade-version
  "Test whether the first version is lower of equal, but just consider the major version,
   i.e. it is possible to upgrade if not already equal."
  [first second]
  (let [first-major (major-version first)
        second-major (major-version second)]
    (<= first-major second-major)))

;; ================================= Interface functions ===============================

(defn migrate-state
  "Migrate state from its existing version to the goal version using the
  supplied migration functions. The migrate functions"
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
