(ns nicheware.platform.utilities.common.core
"
This namespace contains functions that complement those in clojure.core, operating on the main clojure collections and data types.

There are groups of functions within common.core that deal with:

|Function group|Functions|
|---|---|
|slicing collections| [[slice]], [[slice-wrap]], [[remove-slice]], [[remove-slice-wrap]], [[filter-count]]|
|collection inserts| [[insert-before]], [[insert-after]], [[replace-at]]|
|integer functions| [[range-of-range]], [[negate]], [[snap]]|
|general sequence functions| [[rotate-seq]], [[selective-merge]], [[pad-with]], [[seq-of]]|
|collection searching| [[find-first]], [[find-last]], [[find-index]], [[find-last-index]], [[find-nth]], [[replace-leading-nils]]|
|map utility|  [[deep-merge]], [[dissoc-in]], [[index-by]]|
|map transform| [[map-values]], [[map-keys]], [[map-all-keys]]|
|map filtering| [[filter-val]], [[filter-key]], [[filter-remove-val]], [[remove-nil]], [[remove-empty]]|
|vector functions| [[vec-remove-nil]], [[find-by-pred]], [[find-element]], [[replace-element-by-pred]], [[replace-element]]|
|string functions| [[after]], [[before]], [[before-last]], [[str-to-int]]|
|cross-platform functions| [[rand-uuid]], [[parse-int]], [[current-time-millis]], [[edn-read]]|
|function composition| [[compose-fns]]|
|exceptions| [[throw-illegal-arg]]|
|threading macros| [[cond-t]]|

"
  (:require [clojure.string :as str]
            #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as reader])))

;; ================================= Sequence slice functions ======================

(defn ^:no-doc compute-start-end
  "Determines a valid start end index for the given connection, error correcting the given range.
   It will correct errors if start is before first index or end is after the last,
   but nil for all other errors.
  "
  [coll {:keys [start end] :or {start 0} :as slice-range}]
  (let [coll-length (count coll)
        end-range (or end (dec coll-length))]
    ;; Empty seq if the range is not valid
    (if (or (> start end-range)
            (< end-range 0)
            (>= start coll-length))
      nil
      ;; Now we know we can slice some meaningful subset, clamp start and end to be valid
      (let [valid-start (if (< start 0) 0 start)
            valid-end (if (>= end-range coll-length) (dec coll-length) end-range)]
        {:start valid-start :end valid-end}))))

(defn ^:no-doc compute-start-end-count
  "Returns map of valid start, end and subset count"
  ([coll] (compute-start-end-count coll nil))
  ([coll {:keys [subset-count] :as slice-range}]
   (let [{:keys [start end] :as  start-end} (compute-start-end coll slice-range)
         subset (or subset-count (inc (- end start)))]
     ;;(println "compute-start-end-count: start-end: " start-end " subset: " subset)
     (assoc start-end :subset-count subset))))

(defn slice
  "Select the elements from the collection based on the supplied start and end range.
   start and end  are 0 based and inclusive.

   - coll:  Collection to be sliced
   - range: 2 arity argument, either a map ```{:start <default 0> :end <default end-coll>}``` or vector ```[start end]```
   - start: 3 arity argument, start index of slice.
   - end: 3 arity argument, end index of slice.
   - returns: lazy sequence from start to end

   It will attempt to make a valid start and end (eg set start to 0 if < 1 and end to last index in the collection if too large).
   If range is not able to be made valid, returns nil.

   eg:
```clojure
(slice [0 1 2 3 4] 1 3)  => (1 2 3)
(slice [0 1 2 3 4] {:start 3})  => (3 4)
(slice [0 1 2 3 4] [1 7])  => (1 2 3 4)
(slice [0 1 2 3 4] {:start 1 :end 7})  => (1 2 3 4)
(slice [0 1 2 3 4] {:start 3 :end 2}) => nil
```

  "

  ([coll start end] (slice coll {:start start :end end}))
  ([coll range]
   (if (vector? range)
     (slice coll {:start (first range) :end (last range)})

     (let [{:keys [start end] :as range} (compute-start-end coll range)]
       (if (nil? range)
         nil
         (->> coll
              (drop-last (- (count coll) end 1))
              (drop start)))))))


(defn remove-slice
  "Removes the slice specified by ```{:start :end}``` with same defaults and
  semantics as described for [[slice]].

  If range is not able to be clamped to be valid, returns original collection.

  - coll: Collection to be sliced.
  - remove-range: 2 arity argument, either a map ```{:start <default 0> :end <default end-coll>}``` or vector ```[start end]```
  - start: 3 arity argument, index (0 based) of inclusive start of slice to remove.
  - end: 3 arity argument, index (0 based) of inclusive end of slice to remove
  - returns: lazy sequence with slice removed.

   eg:
```clojure
(remove-slice [0 1 2 3 4] 1 3)  => (0 4)
(remove-slice [0 1 2 3 4] {:start 3})  => (0 1 2)
(remove-slice [0 1 2 3 4] {:start 1 :end 7})  => (0)
(remove-slice [0 1 2 3 4] [1 3])  => (0 4)
(remove-slice [0 1 2 3 4] {:start 3 :end 2}) => nil
```
"
  ([coll start end] (remove-slice coll {:start start :end end}))
  ([coll remove-range]
   (let [{:keys [start end] :as range} (compute-start-end coll remove-range)]
     (if (nil? range)
       coll
       (concat (slice coll {:start 0 :end (dec start)})
               (slice coll {:start (inc end)}))))))


(defn slice-wrap
  "Performs a slice but handles wrapping around the vector if the start is after end.

  See [[slice]] for details on arguments.

  eg:
```clojure
(slice-wrap [0 1 2 3 4] {:start 3 :end 1}) => (3 4 0 1)
```
"
  ([coll start end] (slice-wrap coll {:start start :end end}))
  ([coll range]
   ;;(println "slice-wrap(): range: " range)
   (if (vector? range)
     (slice-wrap coll {:start (first range) :end (second range)})

     (let [{:keys [start end]} range]
       (if (and (not (nil? end))
                (> start end))
         ;; Wrap the slice,
         (concat (slice coll {:start start})
                 (slice coll {:start 0 :end end}))

         ;; Normal slice, no wrap required
         (slice coll range))))))

(defn remove-slice-wrap
  "Removes the slice but handles wrapping, so that start can be greater than end, in  which
  case it removes from start to last of vec and from beginning of vec to end.

  See [[remove-slice]] for details on arguments.
eg:

```clojure
(remove-slice-wrap [0 1 2 3 4] {:start 3 :end 1}) => (2)
```
"
  ([coll start end] (remove-slice-wrap coll {:start start :end end}))
  ([coll range]
   ;;(println "remove-slice-wrap(): range: " range)
   (if (vector? range)
     (remove-slice-wrap coll {:start (first range) :end (second range)})

     (let [{:keys [start end]} range]
       (if (and (not (nil? end))
                (> start end))
         ;; Wrap the remove
         (-> coll
             (remove-slice {:start start})
             (remove-slice {:start 0 :end end}))

         ;; Normal slice, no wrap required
         (remove-slice coll range))))))

(defn filter-count
  "Take exactly count elements from the collection, including first and last
   as well as [count - 2] from the remaining collection elements, skipping elements as evenly as possible.

   - coll: Collection to filter from
   - n: Number of elements to extract from collection.
   - returns: lazy sequence with n elements extracted from coll.

eg:
```clojure
(filter-count [1 2 3 4 5 6 7] 4)  =>  [1 3 5 7]
```
"
  [coll n]
  (case n
    ;; Handle special cases first
    1 [(first coll)]
    2 [(first coll) (last coll)]

    (let [size (count coll)
          ;; Exclude last element, how many steps required (round up)
          step (int (Math/ceil (/ (dec size) (dec n))))
          initial-coll (take-nth step (butlast coll))

          ;; We may have fallen short of number required. See how many extra to take
          size-initial (count initial-coll)
          remaining (- (dec n) size-initial)
          taken (* (+ 1 (* step (dec size-initial))))

          ;; The extra will all be taken from the front of the untouched elements (will not touch the last)
          extra (if (> remaining)
                  (take remaining (drop taken coll))
                  [])

          ;; We always want the first and last element, manually need to include the last
          last-element [(last coll)]]
      (concat initial-coll extra last-element))))


(defn nth-items
  "Selects all the given indexes from the supplied collection.

  - coll: Collection from which to extract indexed elements.
  - indexes: collection of indexes. Each is used to extract the nth element from coll
  - returns: lazy-sequence of elements extracted from coll"
  [coll indexes]
  (map #(nth coll %) indexes))

;; ======================= Sequence insert functions ==================================

(defn insert-before
  "Insert the given list into the position just before the specified 0-based index.
   To prepend to list use 0. To append to list use list length or more.

  - coll: Collection to be inserted into.
  - ins-before: Index of element in coll that elements will be inserted before. (0 based)
  - elements: collection of elements to be inserted
  - returns: lazy sequence consisting of coll with elements inserted as specified.
"
  [coll ins-before elements]
  ;; Rely on slice clamping start and en, and returning nil for invalid slices.
  (concat (slice coll {:start  0 :end (dec ins-before)})
          elements
          (slice coll {:start ins-before})))

(defn insert-after
  "Insert the given list after the 0-based index. To insert at the start of the list use
   -1 or insert-before.

  - coll: Collection to be inserted into.
  - ins-after: Index of element in coll that elements will be inserted after. (0 based)
  - elements: collection of elements to be inserted
  - returns: lazy sequence consisting of coll with elements inserted as specified.

"
  [coll ins-after elements]
  (insert-before coll (inc ins-after) elements))

(defn replace-at
  "Replaces all elements starting at the given element with the new elements.
  May extend the sequence if the new elements run longer then the existing sequence.

  - coll: Collection containing elements to be replaced into
  - rep-at: Index of element in coll that replacement to start (0 based)
  - elements: Elements to be used to replace existing elements in coll
  - returns: lazy sequence with coll elements replaced by elements as specified.

eg:
```clojure
  (replace-at [1 2 3 4] 3 [5 6 7]) =>  (1 2 3 5 6 7)
```
"
  [coll rep-at elements]
  (concat (slice coll {:end (dec rep-at)})
          elements
          (slice coll {:start (+ rep-at (count elements))})))

(defn replace-at-wrap
  "Replaces elements in the given collection with the new elements, starting at the given index.
   If the number of new elements would extend past the end of the collection, the replacement wraps,
   starting at the beginning of the collection.

   See [[replace-at]] for details on arguments.
eg
```clojure
(replace-at-wrap [1 2 3 4] 3 [5 6 7]) =>  (6 7 3 5)
```
"
  [coll rep-at elements]
  (let [size (count coll)
        additional-size (count elements)
        first-size (- size rep-at)]
    (if (< first-size additional-size)
      (-> coll
          (replace-at rep-at (take first-size elements))
          (replace-at 0 (drop first-size elements)))
      (replace-at coll rep-at elements))))

;; ============================= Number functions ==============================

(defn range-of-range
  "Returns a lazy clojure range using the given ```{:start ... :end}``` range map
  where the values are inclusive (unlike Clojure range which is exclusive of the end)

  - range-spec: ```{:start <range-start-inclusive> :end <range-end-inclusive}```
  - returns: lazy sequence from start to end inclusive. Empty if either start or end missing.
"
  [{:keys [start end] :as range-spec}]
  (if (not (or  (nil? start) (nil? end)))
    (range start (inc end))
    []))

(defn negate
  "Negate the number (int or real)

  - number: Int or real number to be negated.
  - returns: negated number."
  [number]
  (- 0 number))

(defn snap
  "Snaps the number to its biggest magnitude from 0. This means a ceiling for positive
   numbers and a floor for negative numbers

  - number: integer to be snapped.
  - returns: snapped integer"
  [number]
  (int (if (> number 0) (Math/ceil number) (Math/floor number))))


;; =========================== General sequence functions =============================

(defn rotate-seq
  "Rotates the collection so that the selected index is the new first item in the collection.

  - index: 0 based index at which rotation to start in coll
  - coll: collection to be rotated.
  - returns: lazy sequence of rotated collection. Maps will return list of [key, value] vectors.

eg:
```clojure
  (rotate-seq 2 [0 1 2 3 4 5]) => (2 3 4 5 0 1)
```

"
  [index coll]
  (concat (drop index coll) (take index coll)))

(defn seq-of
  "Creates a lazy sequence filled with the given value n times.

  - value: Value to be repeated.
  - n: Number of times to repeat value
  - returns: Lazy sequence with value repeated n times.
"
  [value n]
  (take n (repeatedly #(identity value))))

(defn pad-with
  "Pads the collection with the given value to make up to the final count.

  - seq: Collection to be padded.
  - value: Value to pad with.
  - new-size: New size of seq, to be padded to this size using value.
  - returns: Newly padded lazy sequence. Original collection returned if new-size less than or equal to current seq size.
    Maps will return a sequence of vectors with [key value] then padded with the pad value.

eg:
```clojure
(pad-with [1 2 3] 0 6) => (1 2 3 0 0 0)
```
"
  [seq value new-size]
  (let [size (count seq)]
    (if (>= size new-size)
      seq
      (concat seq (seq-of value (- new-size size))))))

(defn selective-merge
  "Accept two collections and a truthy collection, all with the same number of values
   and replaces the first with the second, only if the matching selection sequence has is true. Otherwise retains the
   first collection.

  - fst: First collection, values will be used if selection entry is false.
  - snd: Second collection, values will be used if selection entry is true.
  - selections: Same size as fst and snd, with true, false values, used as described above.
  - returns: lazy sequence, same size as fst and snd, with merged values according to selections.
    Maps will retrurn elements as [key value] vectors in the lazy sequence.

eg:
```clojure
(selective-merge [1 2 3 4] [5 6 7 8] [true false true false]) => (5 2 7 4)
```
"
  [fst snd selections]
  (map #(or (and %3 %2) %1) fst snd selections))


(defn is-empty?
  "Returns true for nil or empty collection. Any non-seqable data type will not be true.

  - v: data element to be tested.
  - returns: true if v is nil or empty collection. False also if non-seqable data type."
  [v]
  (or (nil? v) (and (seqable? v) (empty? v))))

(defn max-length
  "Create a predicate function to ensure maximum length of string or collection is max.

  - max: Integer value to be used in predicate as max length.
  - returns: predicate function accepting a countable data structure, returning true if count is less than or equal to max.

eg:
```clojure
(filter (max-length 5) [\"123456\" \"123\"]) => (\"123\")
```
"
  [max]
  (fn [str]
    (<= (count str) max)))

(defn min-length
  "Create a predicate function to ensure minimum length of string or collection is min.

  - min: Integer value to be used in predicate as min length.
  - returns: predicate function accepting a countable data structure, returning true if count is greater than or equal to min.


eg:
```clojure
(filter (min-length 5) [\"123456\" \"123\"]) => (\"123456\")
```
"
  [min]
  (fn [str]
    (>= (count str) min)))


;; =========================== Sequence index functions =================================

(defn find-first
  "Find first value that satisfies predicate.

  - coll: Collection to be searched.
  - pred: Predicate function accepting a value from coll, returning true if matches required condition.
  - returns: First element from coll that matches the predicate. nil if nothing found.

"
  [coll pred]
  ;;(println "find-first(): coll: " coll)
  (first (filter pred coll)))

(defn find-last
  "Find last value that satisfies predicate.

  - coll: Collection to be searched.
  - pred: Predicate function accepting a value from coll, returning true if matches required condition.
  - returns: Last element from coll that matches the predicate. Will examine all elements in coll. nil if no match.

"
  [coll pred]
  ;;(println "find-list(): coll: " coll)
  (last (filter pred coll)))

(defn find-indexes-by-pred
  "Find all the indexes of the elements in the collection matching the given predicate.

  - coll: Collection to be searched.
  - pred: Predicate function accepting a value from coll, returning true if matches required condition.
  - returns: Lazy sequence of indexes of elements from coll that match the predicate. Empty sequence if no match.

"
  [coll pred]
  ;;(println "find-index-by-pred(): coll: " coll)
  (keep-indexed #(if (pred %2) %1) coll))

(defn find-index-by-pred
  "Find the first index of the element in the collection, using the given predicate.

  - coll: Collection to be searched.
  - pred: Predicate function accepting a value from coll, returning true if matches required condition.
  - returns: Index of first element from coll that matches the predicate. nil if no match.


"
  [coll pred]
  ;;(println "find-index-by-pred(): coll: " coll)
  (first (find-indexes-by-pred coll pred)))

(defn find-index
  "Find the first index of the given element in the collection, using the given compare-fn (which defaults to equality).

  - coll: Collection to be searched.
  - element: Element to compare to elements in collection until match is found.
  - compare-fn: Optional 3-arity argument. Function used to match element with those in coll. It should take two args, which will be the item in the list and the element to match.
    If not present, then an equality comparison is performed.
  - returns: The index of the first element in coll which results in true from the compare-fn. nil if no match.

eg:

```clojure
(find-index [10 20 30 40] 30)  => 2
```

"
  ([coll element] (find-index coll element #(= %1 %2)))
  ([coll element compare-fn]
   ;;(println "find-index(), element: " element " coll")
   (find-index-by-pred coll #(compare-fn % element))))

(defn find-indexes
  "Find the all indexes of the element in the collection, using the given compare-fn (which defaults to equality)

  - coll: Collection to be searched.
  - element: Element to compare to elements in collection.
  - compare-fn: Optional 3-arity argument. Function used to match element with those in coll. It should take two args, which will be the item in the list and the element to match.
    If not present, then an equality comparison is performed.
  - returns: Lazy sequence of all the indexes of elements in coll which result in true from the compare-fn. Empty sequence if no match.

  eg:
  ```clojure
  (find-index [10 20 30 40 30] 30)  => (2, 4)
  ```
"
  ([coll element] (find-indexes coll element #(= %1 %2)))
  ([coll element compare-fn]
   (find-indexes-by-pred coll #(compare-fn % element))))

(defn find-last-index
  "Find the index of the matching element in the collection, using the given compare-fn (which default to equality)

  - coll: Collection to be searched.
  - element: Element to compare to elements in collection for match.
  - compare-fn: Optional 3-arity argument. Function used to match element with those in coll. It should take two args, which will be the item in the list and other the element to match.
    If not present, then an equality comparison is performed.
  - returns: The index of the last element in coll which results in true from the compare-fn. nil if no match.


```clojure
(find-last-index [10 20 30 40 30] 30)  => 4
```

"
  ([coll element] (find-last-index coll element #(= %1 %2)))
  ([coll element compare-fn]
   (last (keep-indexed #(if (compare-fn %2 element) %1) coll))))

(defn find-last-index-by-pred
  "Find the last index of the element in the collection, using the given predicate.

  - coll: Collection to be searched.
  - pred: Predicate function accepting a value from coll, returning true if matches required condition.
  - returns: Last index of first element from coll that matches the predicate. nil if no match.

"
  [coll pred]
  (last (keep-indexed #(if (pred %2) %1) coll)))

(defn find-nth
  "Will return nth element of sequence, or return nil if the index is < 0 or >= number elements, rather than exception
   returned by clojure core nth.

  - seq: sequence to access. Must return true to sequential?
  - index: 0 based index of element in seq to return.
  - returns: nth element in sequence or nil if invalid index.

"
  [seq index]
  (if (or (< index 0) (> index (count seq)))
    nil
    (nth seq index)))

(defn replace-leading-nils
  "Replaces all leading nils in the collection with the given value.

  - coll: Collection to be examined for nils.
  - val: Value to replace leading nils with.
  - return: vector of values in collection with leading nils replaced by val.
    If coll is a map it  will return a vector of vectors [key value], with no replacements.

eg
```clojure
(replace-leading-nils [nil nil 1 2 3] 0) => [0 0 1 2 3]
```
"
  [coll val]
  (loop [result []
         col coll
         found-non-nil false
         v val]
    (if (empty? col)
      result
      (let [curr (first col)
            non-nil (or found-non-nil (not (nil? curr)))
            new-val (if non-nil curr v)
            new-result (conj result new-val)]
        (recur new-result (rest col) non-nil v)))))

(defn replace-trailing-nils
  "Replaces all trailing nils in the collection with the given value.

  - coll: Collection to be examined for nils.
  - val: Value to replace trailing nils with.
  - return: vector of values in collection with trailing nils replaced by val.
    If coll is a map it will return a vector of vectors [key value], with no replacements.

eg:
```clojure
  (replace-trailing-nils [1 2 3 nil nil] 0) => (1 2 3 0 0)
```
"
  [coll val]
  (reverse (replace-leading-nils (reverse coll) val)))

;; ========================== Utility map functions ============================

(defn dissoc-in
  "Performs a dissoc of the nested key. It only removes the last key in the path of keys.

   - hmap: Map to be modified.
   - key-path: Sequence of keys, defining nested path in map.
   - returns: New map where final key in key-path has been removed.

eg:
```clojure
(dissoc-in {:one {:two {:three 3 :four 4}}} [:one :two :three]) =>   {:one {:two {:four 4}}}
```
"
  [hmap key-path]
  ;;(println "dissoc-in(): key-path: " key-path)
  (if (= 1 (count key-path))
    (dissoc hmap (first key-path))
    (let [updated (update-in hmap (butlast key-path) dissoc (last key-path))]
      ;;(println "dissoc-in(): key-path: " key-path " butlast: " (butlast key-path))
      updated)))

(defn assoc-in-thread-last
  "Version of assoc-in for use with ->> where the collection must be the last item.  Just does a simple reordering of the arguments.

   - keys: Sequence of keys identifying new key path to add to map.
   - value: New value to be associated with final key in the path.
   - hash-map: Existing map that key to be added to.
   - returns: New map with addition of specified nested key and value.

"
  [keys value hash-map]
  (assoc-in hash-map keys value))

(defn deep-merge
  "Like merge, but merges maps recursively. At each level in any nested maps, keys in the initial map will be replaced
   by keys from subsequent maps.

   - Variable number of arguments, each being a map to be merged from left-to-right
   - returns: New map which is deep merge of all argument maps.

eg:
```clojure
(deep-merge {:one 1 :two 2 :nested {:three 3 :nested {:four 4 :five 5}}}
            {:two 4 :nested {:three 4 :nested {:four 5}}})

=>

{:one 1 :two 4 :nested {:three 4 :nested {:four 5 :five 5}}
```
"
  [& maps]
  (if (every? map? maps)
    (apply merge-with deep-merge maps)
    (last maps)))

(defn index-by
  "Generates a map indexed by the value identified by the key applied to each element in the collection.
   Can be used to convert a vector of maps to a map indexed by a specific key by supplying the
   key as the function. (from juxt doc examples)

   - coll: Collection of maps.
   - key-fn: Fn used to select an element from each map in the collection. eg Could be just a key.
   - returns: New map with an entry for each element in coll, where the key is the key of the attribute selected by key-fn and the value is the original map.

eg:
```clojure
(index-by [{:id 1} {:id 2}] :id) => {1 {:id 1} 2 {:id 2}}
```
"
  [coll key-fn]
  (into {} (map (juxt key-fn identity) coll)))

;; ==================== Map transform functions ===================

(defn map-keys
  "Transforms the top-level keys in the map using the given function.
  Uses for-map. (14.564 usec).

  - fn: Function used to transform the keys. Takes a single argument, the existing key, returning a new key value.
  - value-map: Map to be transformed.
  - returns: New map, with all top-level keys in value-map transformed by fn.
    Handles nil value-map, returning nil
"
  [map-fn value-map]
  (if (empty? value-map)
    value-map
    (reduce-kv (fn [modified-map k value]
                 (assoc modified-map (map-fn k) value))
               {} value-map)))

(defn map-all-keys
  "Transforms the keys in the map and nested maps using the given function.

  - fn: Function used to transform the keys. Takes a single argument, the existing key,  returning a new key value.
  - value-map: Map to be transformed.
  - returns: New map, with all keys, including keys in nested maps, transformed by fn.
    Handles nil value-map, returning nil
"
  [map-fn value-map]
  (if (empty? value-map)
    value-map
    (reduce-kv (fn [modified-map k value]
                 (assoc modified-map (map-fn k)
                        (if (map? value)
                          (map-all-keys map-fn value)
                          value)))
               {} value-map)))

(defn map-values
  "Peforms a map over the values of the hash-map, returning a new hashmap with same keys and updated values.

   - map-fn: Function to be applied to all values in the hash-map. Should accept a single argument (value) and return new value.
   - hmap: The hash-map to be traversed
   - returns: New hash map with all values modified by the map-fn.

"
  [map-fn hmap]
  (reduce-kv (fn [modified-map k value]
               (assoc modified-map k (map-fn value)))
             {} hmap))

;; ==================== Map filtering functions ====================

(defn filter-val
  "Returns a map with only the entries where (fn value) is true.

  - fn: Function which should accept a single argument (the value of each map entry) and return boolean true if entry to be kept.
  - value-map: Hash map to be filtered.
  - returns: New map containing only entries where (fn value) is true.

"
  [fn value-map]
  (select-keys value-map
               (for [[key value] value-map :when (fn value)] key)))

(defn filter-key
  "Returns a map with only the entries where (fn key) is true.

  - fn: Function which should accept a single argument (the key of each map entry) and return boolean true if entry to be kept.
  - value-map: Hash map to be filtered.
  - returns: New map containing only entries where (fn key) is true.

"
  [fn value-map]
  (select-keys value-map
               (for [[key value] value-map :when (fn key)] key)))

(defn filter-remove-val
  "Returns a map where any key where (fn val) is true, is removed (0.8 usec)

  - fn: Function which should accept a single argument (the value of each map entry) and return boolean true if entry to be removed.
  - value-map: Hash map to be filtered.
  - returns: New map containing only entries where (fn value) is false.

"
  [fn value-map]
  (apply dissoc value-map
         (for [[key value] value-map :when (fn value)] key)))

(defn filter-remove-key
  "Returns a map where any key where (fn key) is true, is removed (0.8 usec)

  - fn: Function which should accept a single argument (the key of each map entry) and return boolean true if entry to be removed.
  - value-map: Hash map to be filtered.
  - returns: New map containing only entries where (fn key) is false.

"
  [fn value-map]
  (apply dissoc value-map
         (for [[key value] value-map :when (fn key)] key)))

(defn remove-nil
  "Removes all top-level entries with nil values from the map.
   Uses dissoc, so reuse of map data will be maximised (.8 usec)

   - value-map: Map for which entries with nil values to be removed.
   - returns: New map with no nil value entries.
"
  [value-map]
  (filter-remove-val nil? value-map))

(defn remove-empty
  "Removes all top-level entries which are empty collections or nil values from the map.
   Uses dissoc, so reuse of map data will be maximised (.8 usec).

   Uses the is-empty? function from this lib, so will retain values which are non-seqable.

   - value-map: Map for which entries with empty values to be removed.
   - returns: New map with no empty value entries.

"
  [value-map]
  (filter-remove-val is-empty? value-map))

(defn map-remove-nil
  "Removes all top-level entries with nil values from the map.
   Uses dissoc, so reuse of map data will be maximised (.8 usec).

   Alternate name for remove-nil for backwards compatibility."
  [value-map]
  (remove-nil value-map))

;; ============================= Vector functions ===================

(defn vec-remove-nil
  "Removes any entries that are nil from given vector, returning a vector.

  - vec: Vector to have nils removed.
  - returns: new vector containing only the original non-nil values.

eg:
```clojure
(vec-remove-nil [1 nil 3 nil 4]) => [1 3 4]
```
"
  [vec]
  (into [] (filter #(not (nil? %)) vec)))

(defn find-by-pred
  "Finds and returns the first element in the collection that satisfies the given predicate.

  - coll: Collection to be searched.
  - pred: function which accepts a single value (an element from the collection) and returning true for the required element.
  - returns: The first element from the collection that matches the predicate.
    Maps will be tested and returned as vector elements of the form [key value]
    nil if nothing found.

"
  [coll pred]
  (->> coll
       (filter pred)
       (first)))

(defn find-element
  "Finds the element in the collection of maps, which has the specified value for the given key.

   - coll: Collection of maps to be searched.
   - id-key: Key to access in each map of the collection.
   - id: Expected value of the key in map of matching element.
   - returns: Map from coll that has id-key with value of id. nil if not found.

eg:

```clojure
(find-element [{:id 1 :value 10} {:id 2 :value 100}] :id 1) => {:id 1 :value 10}
```
"
  [coll id-key id]
  ;;(println "find-element(): coll: " coll " id-key: " id-key " id: " id)
  (find-by-pred coll #(= id (get % id-key))))

(defn replace-element-by-pred
  "Replaces an element of a vector, given a new one and a predicate fn that must return true on the element to be replaced.
   Will only replace the first element matched.

   - vec: Vector to have element replaced
   - replacement-element: New element to replace the found element.
   - pred: Function taking a single argument (element in the vec), returning true if it is the one to replace.
   - returns: new vector with first matching element replaced by replacement-element. Same as original vector if no match.

  "
  [vec replacement-element pred]
  (let [index (find-index-by-pred vec pred)]
    (if index
      (assoc vec index replacement-element)
      vec)))

(defn replace-element
  "Replaces a map element of a vector, given a new one and the keyword of the unique identifying attribute in the map.

   - vec: vector in which element is to be replaced.
   - replacement-element: New element to put into vector
   - id-key: The key in the element which is to be used as the matching unique identifier.
   - id: Optional id, identifying the element to be replaced. If not supplied will use the id-key value from the given replacement-element.

eg:
```clojure
(replace-element [{:id 1 :value 100} {:id 2 :value 200}] {:id 1 :value 1000} :id)
  => [{:id 1 :value 1000} {:id 2 :value 200}]

(replace-element [{:id 1 :value 100} {:id 2 :value 200}] {:id 1 :value 1000} :id 2)
  => [{:id 1 :value 100} {:id 1 :value 1000}]
```
"
  ([vec replacement-element id-key] (replace-element vec replacement-element id-key (get replacement-element id-key)))
  ([vec replacement-element id-key id]
   (replace-element-by-pred vec replacement-element #(= id (get % id-key)))))

;; ============================= Function manipulation ==================================

(defn compose-fns
  "Performs a compose of the functions in a given collection. It will execute
   the functions in order of first to last in the collection (which is different from the clojure
   compose which executes from right to left.)

  - coll-fns: Collection of functions to be composed, left to right.
  - returns: single function equivalent to composition of given functions.

   eg:
```clojure
  ((compose-fns [inc #(Mth.abs %)]) -1)  => 0
```
"
  [coll-fns]
  (reduce comp (reverse coll-fns)))

;; ================================== String functions ==================================


(defn after
  "Returns the substring after the first occurrence of the given substring.

  - strn: String to be examined.
  - after-value: Substring within strn to search for.
  - returns: substring after 'after-value' or nil if not found.

  eg:
```clojure
  (after \"prefix-end-suffix\" \"-end-\")  =>  \"suffix\"
```
"
  [strn after-value]
  (some->> after-value
           (str/index-of strn)
           (+ (count after-value))
           (subs strn)))

(defn before
  "Returns the substring before the first occurrence of the given substring.

  - strn: String to be examined.
  - before-value: Substring within strn to search for.
  - returns: substring before 'before-value' or nil if not found.

  eg:
```clojure
  (before \"prefix-end-next-end\" \"-end\")  =>  \"prefix\"
```
"
  [strn before-value]
  (some->> before-value
           (str/index-of strn)
           (subs strn 0)))

(defn before-last
  "Returns the substring before the last occurrence of the given substring.

  - strn: String to be examined.
  - before-value: Substring within strn to search for.
  - returns: last substring before 'before-value' or nil if not found.


  eg:
```clojure
  (before-last \"prefix-end-next-end\" \"-end\")  =>  \"prefix-end-next\"
```
"
  [strn before-value]
  (some->> before-value
           (str/last-index-of strn)
           (subs strn 0)))

;; =========================== Cross platform utility functions =========================

(defn rand-uuid
  "Platform independent uuid generator.
  java.util.UUID/randomUUID for Clojure and random-uuid for ClojureScript.

  - returns: random integer
"
  []
  #?(:clj  (java.util.UUID/randomUUID)
     :cljs (random-uuid)))

(defn parse-int
  "Platform independent int parser.

   Will fail on invalid characters, including digits
   with trailing non-digits, which standard Javascript does not.

   - string-int: String representation of integer
   - returns: integer version of string. If no valid integer
     returns nil

   uses:

   - Integer/parseInt for Clojure
   - js/parseInt for ClojureScript."
  [string-int]
  #?(:clj (try
            (Integer/parseInt string-int)
            (catch Exception e nil))

     ;; Javascript ignores trailing non-digits, we want to trap for that.
     :cljs (if (re-matches #"^(-|\+)?(\d+|Infinity)$" string-int)
             (let [result (js/parseInt string-int)]
               (if (js/isNaN result)
                 nil
                 result))
             nil)))

(defn str-to-int
  "Returns the given string value as an integer, using the platform specific parseInt.
   Returns nil if any format errors.

   Alternate name for [[parse-int]].

  - string-value: Value to be converted to an integer
  - returns: Integer value of string, or nil on format errors."
  [string-value]
  (parse-int string-value))

(defn current-time-millis
  "Returns the current time in millis, as appropriate for the platform."
  []
  #?(:clj (System/currentTimeMillis)
     :cljs (.getTime (js/Date.))))

(defn edn-read
  "Cross platform edn string reader.

  - str: edn formatted string to be parsed.
  - returns: Clojure data read from edn string.

"
  [str]
  #?(:clj (edn/read-string str)
     :cljs (reader/read-string str)))

;; ============================== Macros =================================


(defmacro cond-t
  "Macro for use in threading (as expr in ->). It allows a mapping function to be conditionally applied if the predicate function is true.

   - val - value to be tested - will come from the threading macro
   - pred-fn - predicate function which will be given val, or it could be any non-function expression - which would be tested for truthy
   - map-fn - Executed on the value if the pred-fn or expression is true and then returned as threading value.

   Otherwise val returned untouched.

  eg:
```clojure
  (-> end
      (cond-t #(> start %) inc)
      ...)
```
"
  [val pred-fn map-fn]
  `(let [pred-val# (if (fn? ~pred-fn) (~pred-fn ~val) ~pred-fn)]
     (if pred-val#
       (~map-fn ~val)
       ~val)))


;; ====================== Exceptions =====================================

(defn throw-illegal-arg
  "Throws an IllegalArgumentException, as appropriate for the platform.

  - message: Message to be included as part of response.
  - throws for Clojure: java.lang.IllegalArgumentException
  - throws for ClojureSciprt: js/Error with the string \"IllegalArgumentException\" in the message.
"
  [message]
  (throw
   #?(:clj (IllegalArgumentException. message)
      :cljs (js/Error (str "IllegalArgumentException: " message)))
   ))


;; ================= Scratch ======================

(comment
  (def test-map {:a 1 :b 2 :c 3 :d 4 :e 5})

  ;; "Elapsed time: 2335.636127 msecs - 2.34 usecs per operation"
  (time
   (dotimes [_ 1000000]
     (into {} (map (fn [[key value]] [key (inc value)]) test-map))))

  ;; "Elapsed time: 428.405277 msecs - 0.43 usecs per operation"
  (time
   (dotimes [_ 1000000]
     (reduce-kv (fn [modified-map k value] (assoc modified-map k (inc value))) {} test-map)))

  ;; "Elapsed time: 1359.278394 msecs - 1.35 usecs per operation"
  (time
   (dotimes [_ 1000000]
     (zipmap (keys test-map)
             (map inc (vals test-map))))))
