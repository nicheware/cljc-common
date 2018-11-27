(ns nicheware.platform.utilities.common.version
  (:require [nicheware.platform.utilities.common.core :as common]))

;; =================================== Information ==========================
;;
;; Versions assets are assumed to be held in an asset map of the form
;;
;; { <asset-key> {:current <modified-time>
;;                :versions { <modified-time-1> <asset-v1>
;;                            <modified-time-2> <asset-v2>
;;                          }
;;               }
;;
;;   <asset-key-2> ....
;; }
;;

;; ================================= Version reference functions ==============================

(defn make-ref-name-fn
  "Makes a function that will test an element with a reference key item against the name (where name
   will match the first item of the ref-key value)"
  [ref-key name]
  (fn [elem] (= (first (get elem ref-key)) name)))

(defn find-element-with-ref-name
  "Search's a collection for the first element that has a version reference matching the given name.
    Uses ref-key to determine which item in the element map is the version reference, then matches
    name against the first item of this reference."
  [vec ref-key name]
  (common/find-by-pred vec (make-ref-name-fn ref-key name)))


(defn replace-element-with-ref-name
  [vec replacement-element ref-key name]
  (common/replace-element-by-pred vec replacement-element (make-ref-name-fn ref-key name)))

;; ============================= Versioned asset functions =========================================

(defn get-ordered-versions
  "Extracts a order list of versions (from least to highest modified time) from the versioned-asset"
  [{:keys [versions] :as versioned-asset}]
  (sort-by :modified-time (vals versions)))

(defn find-index
  "Looks for current item within the versioned asset.
   If not found, will return nil"
  [versioned-asset {:keys [modified-time] :as element}]
  (println "find-index(): modified-time: " modified-time)
  (-> versioned-asset
      get-ordered-versions
      (common/find-index-by-pred #(= (:modified-time %) modified-time))))

(defn current-version-key
  "Gets the current version key for the specified asset"
  [assets asset-key]
  (get-in assets [asset-key :current]))

(defn get-current-ref
  "Build an asset reference [name version] for the named asset using the current version"
  [assets asset-key]
  [asset-key (current-version-key assets asset-key)])

(defn get-first-asset-ref
  "Access the first asset in the asset map and then extracts the reference to the current version
   for that asset"
  [assets]
  (let [[asset-key _] (first assets)]
    (get-current-ref assets asset-key)))

(defn get-version
  "Gets the specified version of the named asset"
  [assets [asset-key version]]
  (get-in assets [asset-key :versions version]))

(defn get-current
  "Gets the current version of the named curve"
  [assets asset-key]
  (get-version assets [asset-key (current-version-key assets asset-key)]))


(defn get-current-asset
  "Gets the current version of the given versioned asset"
  [{:keys [current versions] :as versioned-asset}]
  ;;(println "get-current-asset(): current: " current " versioned-asset: " versioned-asset)
  (get versions current))

(defn rename-versions
  "Renames each map entry"
  [versions new-name]
  (common/map-values #(assoc % :name new-name) versions))

(defn rename-versioned-asset
  "Will change the top level name of the asset and change the name in all the versions of the asset"
  [{:keys [versions] :as versioned-asset} new-name]
  (-> versioned-asset
      (assoc :name new-name)
      (assoc :versions (rename-versions versions new-name))))

(defn timestamp-element
  "Marks the element with the appropriate modification time"
  ([element] (timestamp-element element false))
  ([element force]
   (println "timestamp-element(): element: " element)
   (assoc element :modified-time (common/current-time-millis))))

(defn version-element
  "Prepare a new element for versioning. Will ensure timestamped and also
   not starred (as is new version)"
  [element]
  (-> element
      timestamp-element
      (assoc :starred false)))

(defn add-version
  "Adds to the version list, if not already in the list, will append to the end.
   Becomes the new current version"
  [versioned-asset element]
  ;;(println "add-version(): element: " element)
  (let [{:keys [modified-time] :as  versioned-element} (version-element element)
        dummy (println "add-version(): modified-time: " modified-time)]
    (-> (if (get-in versioned-asset [:versions modified-time])
          versioned-asset
          (assoc-in versioned-asset [:versions modified-time] versioned-element))
        (assoc :current modified-time))))

(defn replace-current
  "Replaces the current version with the supplied version, but does not change any version number.
   Should be used only when updates are to be made to the current version which should not affect versioning."
  [{:keys [current] :as versioned-asset} asset]
  ;;(println "replace-current(): current: " current)
  (let [updated-asset (assoc asset :modified-time current)]
    (assoc-in versioned-asset [:versions current] updated-asset)))

(defn set-version
  "Sets the current version to the modified-time of the given asset. Assumes this exists already as a version"
  [versioned-asset asset]
  (assoc versioned-asset :current (:modified-time asset)))

(defn mutate-version
  "Updates the version by either replacing the current or adding a new version.
   The action taken depends on whether the current one has the same mutation as passed in. If the same
   we dont bother creating a new version and just replace current."
  [mutation versioned-asset asset]
  ;;(println "mutate-version(): mutation: " mutation " name: " (:name versioned-asset))
  (let [updated-asset (assoc asset :mutation mutation)
        current-mutation (:mutation (get-current-asset versioned-asset))]
    (if (= mutation current-mutation)
      (replace-current versioned-asset updated-asset)
      (add-version versioned-asset updated-asset))))

(defn delete-version
  "Removes the specified version from the versioned asset. Ensure at least one asset and that if the
   deleted asset is current a new asset (first) is selected as current"
  [{:keys [current versions] :as versioned-asset} version]
  (let [updated-versions (dissoc versions version)]
    (if (> (count versions) 1)
      (cond-> versioned-asset
        true (assoc :versions updated-versions)
        (= current version) (assoc :current (first (first updated-versions))))
      versioned-asset)))

(defn delete-asset-version
  "Same as delete version execpt it is given the version to be deleted rather than just
   the version number (modified-time)"
  [versioned-asset {:keys [modified-time] :as asset}]
  (delete-version versioned-asset modified-time))


(defn remove-unused-versions
  "Removes any unused versions. A used version is either current or marked as starred. Optional
   last argument should accept the version and return true is used."
  ([versioned-asset] (remove-unused-versions versioned-asset (constantly false)))
  ([{:keys [current versions] :as versioned-asset} pred-fn]
   ;;(println "remove-unused-versions(): versioned-asset.versions: " versions)
   (let [remove-pred #(not (or (= current (:modified-time %))
                               (:starred %)
                               (pred-fn %)))]
     (assoc versioned-asset :versions (common/filter-remove-val remove-pred versions)))))
