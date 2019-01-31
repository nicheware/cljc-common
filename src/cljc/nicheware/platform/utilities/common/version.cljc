(ns nicheware.platform.utilities.common.version
  "

  This namespace provides a set of functions supporting in-memory versioning of a map of  assets represented
  by a clojure map.

  There are groups of functions within the version namespace that deal with:

  |Function group|Functions|
  |---|---|
  |version reference functions| [[make-ref-name-fn]] [[find-element-with-ref-name]], [[get-current-ref]], [[get-first-asset-ref]], [[replace-element-with-ref-name]]|
  |version access functions| [[get-ordered-versions]], [[find-index]], [[current-version-key]], [[get-version]], [[get-current]], [[get-current-asset]]|
  |version update functions| [[rename-versions]], [[rename-versioned-assets]], [[timestamp-element]], [[version-element]], [[add-version]], [[replace-current]], [[set-version]], [[mutate-version]], [[delete-version]], [[delete-asset-version]], [[remove-unused-versions]]|

  A collection of versioned assets are assumed to be held in an asset map of the form:

  ```clojure
  { <asset-key> {:current <modified-time>
                :name <asset-key>
                :versions { <modified-time-1> {<asset-v1> :name <asset-key> :modified-time <modified-time-1> }
                            <modified-time-2> {<asset-v2> :name <asset-key> :modified-time <modified-time-2> }
                          }
               }

   <asset-key-2> ....
  }
  ```

  Each asset has a unique key (asset-key). The key is used to

  - access the asset in the top level map of different assets
  - usually accessible as the ```:name``` attribute in the full versioned asset
  - usually accessible as the ```:name``` attribute in each of the versions of the asset.

  Versioning is done by having a current version (```:current```) which
  is a key into different versions of the assets. The keys will normally be the time the version was created.

  Functions are provided for adding a new version, removing existing versions etc.

  In this simple implementation each version is a full copy of the assets state (any Clojure data structure).

  It supports moving through past versions (or undo) simply by changing the modified time referenced by ```:current```.

  The functions also support the concept of a version reference, which is a vector of the form

  ```clojure
  [ <asset-key> <version-number>]
  ```

  This would be used as reference pointer in any Clojure data type to reference a particular version of an asset.

  ```version-number``` is normally just the modified time.
  "
  (:require [nicheware.platform.utilities.common.core :as common
             :include-macros true]))


;; ================================= Version reference functions ==============================

(defn make-ref-name-fn
  "Makes a function that will test an element with a reference key item against the name (where name
   will match the first item of the ref-key value, i.e. the asset-key)

   - ref-key: The key in the element that contains the reference.
   - name: The value of the name part (or asset-key) of the reference that the returned function will check
   - returns: ```fn(element)``` where element should have a key of ref-key that is a reference  ```[asset-key version]``` and the function will return true if the reference asset-key equals \"name\"
"
  [ref-key name]
  (fn [elem] (= (first (get elem ref-key)) name)))

(defn find-element-with-ref-name
  "Search's a vector for the first element that has a version reference matching the given name.

  Uses ref-key to determine which item in the element map is the version reference, then matches
  name against the first item of this reference.

  Useful when searching for references to a versioned asset.

  - vec: Vector of elements. ```[ {<ref-key> [<asset-name> <asset-version] ...}, ...]```
  - ref-key: The key within each element in the vector that contains the version reference of the form ```[<asset-key> <version>]```
  - name: The asset key to be matched.
  - returns: The full element from vec which has a reference with that name.

"
  [vec ref-key name]
  (common/find-by-pred vec (make-ref-name-fn ref-key name)))


(defn replace-element-with-ref-name
  "Searches a vector of elements where each element holds a reference ```[name, version]```. It will
   search for the element that references the given name, and replace it with the supplied replacement element.

   - vec: Vector of elements to search. ```[ {<ref-key> [name, version] ...}, ...]```
   - replacement-element: New element to replace the matching element.
   - ref-key: The key in the element that holds the reference ```[name/asset-key, version]```
   - name: The name/asset-key in the reference that is being searched for.
   - returns: New vector with the suitable replacement element.
  "
  [vec replacement-element ref-key name]
  (common/replace-element-by-pred vec replacement-element (make-ref-name-fn ref-key name)))

;; ============================= Versioned asset functions =========================================

(defn get-ordered-versions
  "Extracts an ordered list of versions (from least to highest modified time) from the versioned-asset.

   - versioned-asset: Versioned asset to be searched for versions.  ```{:versions {<mod-key-1> {<asset-1>}}}```
   - returns: vector of versions (from smallest to highest) ```[<asset-1> ...]```]

"
  [{:keys [versions] :as versioned-asset}]
  (sort-by :modified-time (vals versions)))

(defn find-index
  "Treats the versions within the versioned-assets as a vector of version modification times, from smallest to highest,
   and returns the index within that vector of the provided elements modified time.

   - versioned-asset: Versioned asset to be searched for versions.  ```{:versions {<mod-key-1> {<asset-1>}}}```
   - element: Uses the ```:modified-time``` attribute as the value to find the index of.
   - returns index or nil if not found.

   "
  [versioned-asset {:keys [modified-time] :as element}]
  ;;(println "find-index(): modified-time: " modified-time)
  (-> versioned-asset
      get-ordered-versions
      (common/find-index-by-pred #(= (:modified-time %) modified-time))))

(defn current-version-key
  "Gets the current version key for the specified asset

  - assets: Map of assets, keyed by assets key, where each asset is versioned. ```{<asset-1-key> {:current <mod-time> :versions {:mod-time-1 <asset-v1> ...}}}```
  - asset-key: Key into the asset map
  - returns: The ```:current``` of the asset matching the given key. nil if asset not present.

"
  [assets asset-key]
  (get-in assets [asset-key :current]))

(defn get-current-ref
  "Build an asset reference [name version] for the named asset using the current version

  - assets: Maps of assets, keyed by assets key, where each asset is versioned. ```{<asset-1-key> {:current <mod-time> :versions {:mod-time-1 <asset-v1> ...}}}```
  - asset-key: Key into the asset map
  - returns: A reference to the current version of the asset. Format is ```[asset-key version]```. version is usually modified time.
    if asset not in assets, reference will have nil as the version.

"
  [assets asset-key]
  [asset-key (current-version-key assets asset-key)])

(defn get-first-asset-ref
  "Access the first asset in the asset map and then extracts the reference to the current version
   for that asset

  - assets: Maps of assets, keyed by assets key, where each asset is versioned. ```{<asset-1-key> {:current <mod-time> :versions {:mod-time-1 <asset-v1> ...}}}```
  - returns: A reference to the current version of the first asset. Format is ```[asset-key version]```. version is usually modified time.

"
  [assets]
  (let [[asset-key _] (first assets)]
    (get-current-ref assets asset-key)))

(defn get-version
  "Gets the specified version of the named asset

  - assets: Maps of assets, keyed by assets key, where each asset is versioned. ```{<asset-1-key> {:current <mod-time> :versions {:mod-time-1 <asset-v1> ...}}}```
  - reference: Vector of the asset-key (or name) and the required version (usually modified-time). ```[asset-key version]```
  - returns: the asset with the required key and version.

"
  [assets [asset-key version]]
  (get-in assets [asset-key :versions version]))

(defn get-current
  "Gets the current version of the asset with the given key.

  - assets: Maps of assets, keyed by asset key, where each asset is versioned. ```{<asset-1-key> {:current <mod-time> :versions {:mod-time-1 <asset-v1> ...}}}```
  - asset-key: Key of the asset for which the current version required.
  - returns: The current version of the asset.

"
  [assets asset-key]
  (get-version assets [asset-key (current-version-key assets asset-key)]))


(defn get-current-asset
  "Gets the current version of the given versioned asset.

   - versioned-asset: Versioned asset to be searched for versions.  ```{:current <mod-time> :versions {<mod-key-1> {<asset-1>}}}```
   - returns: The current version of the asset.
"
  [{:keys [current versions] :as versioned-asset}]
  ;;(println "get-current-asset(): current: " current " versioned-asset: " versioned-asset)
  (get versions current))

(defn rename-versions
  "Renames element in the versions map, modifying the ```:name``` attribute to the new name.

   - versions: Versions map, each entry to have name updated.  ```{<mod-key-1> {:name <asset-key> ...}, <mod-key-2> {..} ...}```
   - new-name: New name to use for the asset, and hence for each version.
   - returns: updated version map where the name in the versions are modified to the new name.
"
  [versions new-name]
  (common/map-values #(assoc % :name new-name) versions))

(defn rename-versioned-asset
  "Will change the top level name of the asset and change the name in all the versions of the asset.

   - versioned-asset: Versioned asset to be searched for versions.  ```{:current <mod-time> :name <asset-key> :versions {<mod-key-1> {:name <asset-key> ...}, <mod-key-2> {..} ...}}```
   - new-name: New name to use for the asset, and hence for each version.
   - returns: updated versioned-asset map where the top level name and the name in the versions are modified to the new name.
"
  [{:keys [versions] :as versioned-asset} new-name]
  (-> versioned-asset
      (assoc :name new-name)
      (assoc :versions (rename-versions versions new-name))))

(defn timestamp-element
  "Marks the element with the appropriate modification time.

  - element: Map in which a new ```:modified-time``` keyword to be added, if required.
  - force: optional 2 arity argument. If true will update ```:modified-time``` even if it exists. false by default.
  - returns: map with ```:modified-time``` set to current time in millis if not set or force = true.
"
  ([element] (timestamp-element element false))
  ([element force]
   ;;(println "timestamp-element(): element: " element)
   (if (or force (:modified-time element))
     element
     (assoc element :modified-time (common/current-time-millis)))))

(defn version-element
  "Prepare a new element for versioning. Will ensure timestamped and also
   not starred (as is new version)

   - element: Map which represent the version of an asset to be placed into list of versions.
   - returns: same element, but with appropriate ```:modified-time``` and no ```:starred``` attribute.
"
  [element]
  (-> element
      timestamp-element
      (common/cond-t :starred #(assoc % :starred false))))

(defn add-version
  "Adds to the version list, if not already in the list, will append to the end.
   Becomes the new current version.

   - versioned-asset: Existing versioned asset, with map of current versions. ```{:current <mod-key-n> :versions {<mod-key-1> {<asset-1> ...}}}```
   - element: New element to add to the list. Will set the ```:modified-time``` to be current time if not already set
   - returns: versioned-asset with element added as the latest version (i.e. ```:current``` updated)
"
  [versioned-asset element]
  ;;(println "add-version(): element: " element)
  (let [{:keys [modified-time] :as  versioned-element} (version-element element)
        ;;dummy (println "add-version(): modified-time: " modified-time)
        ]
    (-> (if (get-in versioned-asset [:versions modified-time])
          versioned-asset
          (assoc-in versioned-asset [:versions modified-time] versioned-element))
        (assoc :current modified-time))))

(defn replace-current
  "Replaces the current version with the supplied version, but does not change any version number.

  - versioned-asset: Existing versioned asset, with map of current versions. ```{:current <mod-key> :versions {<mod-key-1> {<asset-1>}}}```
  - asset: New asset to replace the existing current.
  - returns: Updated versioned-asset.

  Should be used only when updates are to be made to the current version which you don't want to affect versioning."
  [{:keys [current] :as versioned-asset} asset]
  ;;(println "replace-current(): current: " current)
  (let [updated-asset (assoc asset :modified-time current)]
    (assoc-in versioned-asset [:versions current] updated-asset)))

(defn set-version
  "Sets the current version to the ```:modified-time``` of the given asset. Assumes this exists already as a version.

  - versioned-asset: Existing versioned asset, with map of current versions. ```{:current <mod-key> :versions {<mod-key-1> {<asset-1>}}}```
  - asset: New asset to whose modified-time is to become the current version.
  - returns: Updated versioned-asset.

"
  [versioned-asset asset]
  (assoc versioned-asset :current (:modified-time asset)))

(defn mutate-version
  "Updates the version by either replacing the current or adding a new version.
   The action taken depends on whether the current one has the same mutation as passed in. If the same
   we dont bother creating a new version and just replace current.

   - mutation: The data to be added to the asset using the ```:mutation``` key.
   - versioned-asset: Existing versioned asset, with map of current versions. ```{:current <mod-key> :versions {<mod-key-1> {<asset-1>}}}```
   - asset: asset data to have mutation added.
   - returns: updated versioned-asset with the mutated asset added as a new and latest version, or if the current assest has the same mutation,
     just replaced the current version with the new asset data.

"
  [mutation versioned-asset asset]
  ;;(println "mutate-version(): mutation: " mutation " name: " (:name versioned-asset))
  (let [updated-asset (assoc asset :mutation mutation)
        current-mutation (:mutation (get-current-asset versioned-asset))]
    (if (= mutation current-mutation)
      (replace-current versioned-asset updated-asset)
      (add-version versioned-asset updated-asset))))

(defn delete-version
  "Removes the specified version from the versioned asset. Ensures at least one asset version remains, (i.e, you cannot delete the last version) and that if the
   deleted asset is current a new asset (first) is selected as current.

  - versioned-asset: Existing versioned asset, with map of current versions. ```{:current <mod-key> :versions {<mod-key-1> {<asset-1>}}}```
  - version: The version to be removed, typically the modified time.
  - returns: updated versioned-asset.
"
  [{:keys [current versions] :as versioned-asset} version]
  (let [updated-versions (dissoc versions version)]
    (if (> (count versions) 1)
      (cond-> versioned-asset
        true (assoc :versions updated-versions)
        (= current version) (assoc :current (first (first updated-versions))))
      versioned-asset)))

(defn delete-asset-version
  "Same as delete version execpt it is given the version to be deleted rather than just
   the version number (modified-time).

   If the version is the current one, then current becomes on the remaining versions.

  - versioned-asset: Existing versioned asset, with map of current versions. ```{:current <mod-key> :versions {<mod-key-1> {<asset-1-data>}}}```
  - asset: Asset to be removed. Must include a ```:modified-time``` key, to identify the version to be deleted.
  - returns: updated versioned-asset, with version removed and ```:current``` updated if the current was deleted.

"
  [versioned-asset {:keys [modified-time] :as asset}]
  (delete-version versioned-asset modified-time))


(defn remove-unused-versions
  "Removes any unused versions. A used version is either current or marked as starred (has a ```:starred``` true attribute) or returns true to the optional given predicate function.

  - versioned-asset: Existing versioned asset, with map of current versions. ```{:current <mod-1> :versions {<mod-key-1 {:starred <bool> <asset-1-data>}}}```
  - pred-fn:  optional last argument should accept the version and return true if used. Defaults to a function that is always false
  - returns: versioned-asset with unused versions removed.

"
  ([versioned-asset] (remove-unused-versions versioned-asset (constantly false)))
  ([{:keys [current versions] :as versioned-asset} pred-fn]
   ;;(println "remove-unused-versions(): versioned-asset.versions: " versions)
   (let [remove-pred #(not (or (= current (:modified-time %))
                               (:starred %)
                               (pred-fn %)))]
     (assoc versioned-asset :versions (common/filter-remove-val remove-pred versions)))))
