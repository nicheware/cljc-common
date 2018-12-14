(ns nicheware.platform.utilities.common.core
"
Functions that complement those in clojure.core, operating on the main clojure collections and data types.

There are groups of functions within core that deal with:

- slicing sequences: eg [[slice]], [[slice-wrap]], [[remove-slice]], [[remove-slice-wrap]], [[filter-count]]

- sequence inserts: eg [[insert-before]], [[insert-after]], [[replace-at]]

- integer functions: eg [[range-of-range]], [[negate]], [[snap]]

- general sequence functions: eg [[rotate-seq]], [[selective-merge]], [[pad-with]], [[seq-of]]

- sequence searching: eg [[find-first]], [[find-last]], [[find-index]], [[find-last-index]], [[find-nth]], [[replace-leading-nils]]

- map functions: eg [[map-values]], [[deep-merge]], [[dissoc-in]], [[index-by]]

- map filtering: eg [[filer-val]], [[filter-key]], [[filter-remove-val]], [[remove-nil]], [[remove-empty]], [[transform-keys]], [[map-all-kets]]

- vector functions: eg [[vec-remove-nil]], [[find-by-pred]], [[find-element]], [[replace-element-by-pred]], [[replace-element]]

- string functions: eg [[before]], [[before-last]], [[str-to-int]]

- cross-platform functions: [[rand-uuid]], [[parse-int]], [[current-time-millis]], [[edn-read]]

- function composition: eg [[compose-fns]]

- exceptions: [[throw-illegal-arg]]

- macros: [[cond-t]]
"
  (:require [clojure.string :as str]
            [plumbing.core :as pc]
            #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as reader])))

;; ==================== Type conversion functions ==================

(defn str-to-int
  "Returns the given string value as an integer, using the platform specific parseInt.
   Returns nil if any format errors."
  [string-value]
  (try
    (. Integer parseInt string-value)
    (catch NumberFormatException _ nil)))

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
  "Select the elements from the sequence based on the supplied start and end range
   Indexes are 0 based and inclusive.

   - coll:  Sequence to be slice
   - slice-range:  ```{:start <default 0> :end <default end-coll>}```
   - returns: sequence from start to end.

   It will attempt to make a valid start and end (eg set start to 0 if < 1 and end to end collection if too large).
   If range is not able to be made valid, returns nil.

   eg:
```clojure
(slice [0 1 2 3 4] 1 3)  => (1 2 3)
(slice [0 1 2 3 4] {:start 3})  => (3 4)
(slice [0 1 2 3 4] {:start 1 :end 7})  => (1 2 3 4)
(slice [0 1 2 3 4] {:start 3 :end 2}) => nil
```
   Returns a list.
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
  semantics as described for slice.

  If range is not able to be clamped to be valid, returns original collection.
   eg:
```clojure
(remove-slice [0 1 2 3 4] 1 3)  => (0 4)
(remove-slice [0 1 2 3 4] {:start 3})  => (0 1 2)
(remove-slice [0 1 2 3 4] {:start 1 :end 7})  => (0)
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
  case it removes from start to last of vec and from beginning of vec to start. eg:

```clojure
(remove-slice-wrap [0 1 2 3 4] {:start 3 :end 1}) => (2)
```
"
  ([coll start end] (slice-wrap coll {:start start :end end}))
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
   as well as [count - 2] from the remaining collection elements, skipping elements as evenly as possible. eg:
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
  "Selects all the given indexes from the supplied collection."
  [coll indexes]
  (map #(nth coll %) indexes))

;; ======================= Sequence insert functions ==================================

(defn insert-before
  "Insert the given list into the position just before the specified 0-based index.
   To prepend to list use 0. To append to list use list length or more."
  [coll ins-before elements]
  ;; Rely on slice clamping start and en, and returning nil for invalid slices.
  (concat (slice coll {:start  0 :end (dec ins-before)})
          elements
          (slice coll {:start ins-before})))

(defn insert-after
  "Insert the given list after the 0-based index. To insert at the start of the list use
   -1 or insert-before."
  [coll ins-after elements]
  (insert-before coll (inc ins-after) elements))

(defn replace-at
  "Replaces all elements starting at the given element with the new elements.
  May extend the sequence if the new elements run longer then the existing sequence. eg:
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
   If the number of new elemenets would extend past the end of the collection, the replacement wraps,
   starting at the beginning of the collection. eg
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
"
  [range-spec]
  (range (:start range-spec) (inc (:end range-spec))))

(defn negate
  "Negate the number (int or real)"
  [number]
  (- 0 number))

(defn snap
  "Snaps the number to its biggest magnitude from 0. This means a ceiling for positive
   number and a floor for negative numbers"
  [number]
  (int (if (> number 0) (Math/ceil number) (Math/floor number))))


;; =========================== General sequence functions =============================

(defn rotate-seq
  "Rotates the sequence so that the selected index is the new first item in the collection. eg:
```clojure
  (rotate-seq 2 [0 1 2 3 4 5]) => (2 3 4 5 0 1)
```
  Returns a list.
"
  [index sequ]
  (concat (drop index sequ) (take index sequ)))

(defn seq-of
  "Creates a sequence filled with the given value n times"
  [value n]
  (take n (repeatedly #(identity value))))

(defn pad-with
  "Pads the sequence with the given value to make up to the final count. eg:
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
  "Accept two sequences and a truthy sequence, all with the same number of values
   and replaces the first with the second, only if the matching selection sequence has is true. Otherwise retains the
   first collection. eg:
```clojure
(selective-merge [1 2 3 4] [5 6 7 8] [true false true false]) => (5 2 7 4)
```
"
  [fst snd selections]
  (map #(or (and %3 %2) %1) fst snd selections))


(defn is-empty?
  "Returns true for nil or empty sequence. Any non-sequence will not be true."
  [v]
  (or (nil? v) (and (sequential? v) (empty? v))))

(defn max-length
  "Create a predicate function to ensure maximum length of string or collection is max. eg:
```clojure
(filter (max-length 5) [\"123456\" \"123\"]) => (\"123\")
```
"
  [max]
  (fn [str]
    (<= (count str) max)))

(defn min-length
  "Create a predicate function to ensure minimum length of string or collection is min. eg:
```clojure
(filter (min-length 5) [\"123456\" \"123\"]) => (\"123456\")
```
"
  [min]
  (fn [str]
    (>= (count str) min)))


;; =========================== Sequence index functions =================================

(defn find-first
  "Find first value that satisfies predicate"
  [coll pred]
  ;;(println "find-first(): coll: " coll)
  (first (filter pred coll)))

(defn find-last
  "Find last value that satisfies predicate"
  [coll pred]
  ;;(println "find-list(): coll: " coll)
  (last (filter pred coll)))

(defn find-indexes-by-pred
  "Find the first index of the element in the collection, using the given predicate, which should
   accept an element from the collection and return true or false"
  [coll pred]
  ;;(println "find-index-by-pred(): coll: " coll)
  (keep-indexed #(if (pred %2) %1) coll))

(defn find-index-by-pred
  "Find the first index of the element in the collection, using the given predicate, which should
   accept an element from the collection and return true or false"
  [coll pred]
  ;;(println "find-index-by-pred(): coll: " coll)
  (first (find-indexes-by-pred coll pred)))

(defn find-index
  "Find the first index of the given element in the collection, using the given compare-fn (which defaults to equality).

   compare-fn should take two args, which will be the item in the list, then the element. eg:
```clojure
(find-index [10 20 30 40] 30)  => 2
```
   Returns nil if no match.
"
  ([coll element] (find-index coll element #(= %1 %2)))
  ([coll element compare-fn]
   ;;(println "find-index(), element: " element " coll")
   (find-index-by-pred coll #(compare-fn % element))))

(defn find-indexes
  "Find the all indexes of the element in the collection, using the given compare-fn (which default to equality)
   compare-fn should take two args, which will be the item in the list, then the element. eg:
```clojure
(find-index [10 20 30 40 30] 30)  => (2, 4)
```
   Returns empty list if nothing found.
"
  ([coll element] (find-indexes coll element #(= %1 %2)))
  ([coll element compare-fn]
   (find-indexes-by-pred coll #(compare-fn % element))))

(defn find-last-index
  "Find the last index of the element in the collection, using the given compare-fn (which default to equality)
   compare-fn should take two args, which will be the item in the list, then the element
```clojure
(find-last-index [10 20 30 40 30] 30)  => 4
```
   Return nil if nothing found.
"
  ([coll element] (find-last-index coll element #(= %1 %2)))
  ([coll element compare-fn]
   (last (keep-indexed #(if (compare-fn %2 element) %1) coll))))

(defn find-last-index-by-pred
  "Find the last index of the element in the collection, using the given predicate."
  [coll pred]
  (last (keep-indexed #(if (pred %2) %1) coll)))

(defn find-nth
  "Will return nth element of sequence, or return nil if the index is < 0 or >= number elements"
  [seq index]
  (if (or (< index 0) (> index (count seq)))
    nil
    (nth seq index)))

(defn replace-leading-nils
  "Replaces all leading nils in the sequence with the given value. eg
```clojure
(replace-leading-nils [nil nil 1 2 3] 0) => [0 0 1 2 3]
```
   Returns a vector.
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
  "Replaces all trailing nils in the sequence with the given value.
```clojure
  (replace-trailing-nils [1 2 3 nil nil] 0) => (1 2 3 0 0)
```
  Returns a list.
"
  [coll val]
  (reverse (replace-leading-nils (reverse coll) val)))

;; ========================== Utility map functions ============================

(defn map-values
  "Peforms a map over the values of the hash-map, returning a new hashmap with same keys and updated values"
  [map-fn hmap]
  (reduce-kv (fn [modified-map k value] (assoc modified-map k (map-fn value))) {} hmap))

(defn dissoc-in
  "Performs an expected dissoc of the nested keys. eg:
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
  "Version of assoc in for use with ->> where the collection is the last item.  eg:
```clojure
   (->> col
        (merge data)
        (assoc-in-thread [:parent :child] value))
```

"
  [keys value hash-map]
  (assoc-in hash-map keys value))

(defn deep-merge
  "Like merge, but merges maps recursively. At each level in any nested maps, keys in the initial map will be replaced
   by keys from subsequent maps eg:
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
   key as the function. (from juxt doc examples) eg:
```clojure
(index-by [{:id 1} {:id 2}] :id) => {1 {:id 1} 2 {:id 2}}
```
"
  [coll key-fn]
  (into {} (map (juxt key-fn identity) coll)))

;; ==================== Map filtering functions ====================

(defn filter-val
  "Returns a map with only the entries where (fn value) is true"
  [fn value-map]
  (select-keys value-map
               (for [[key value] value-map :when (fn value)] key)))

(defn filter-key
  "Returns a map with only the entries where (fn key) is true"
  [fn value-map]
  (select-keys value-map
               (for [[key value] value-map :when (fn key)] key)))

(defn filter-remove-val
  "Returns a map where any key where (fn val) is true, is removed (0.8 usec)"
  [fn value-map]
  (apply dissoc value-map
         (for [[key value] value-map :when (fn value)] key)))

(defn filter-remove-key
  "Returns a map where any key where (fn key) is true, is removed (0.8 usec)"
  [fn value-map]
  (apply dissoc value-map
         (for [[key value] value-map :when (fn key)] key)))

(defn remove-nil
  "Removes all top-level entries with nil values from the map.
   Uses dissoc, so reuse of map data will be maximised (.8 usec)"
  [value-map]
  (filter-remove-val nil? value-map))

(defn remove-empty
  "Removes all top-level entries which are empty collections or nil values from the map.
   Uses dissoc, so reuse of map data will be maximised (.8 usec)"
  [value-map]
  (filter-remove-val is-empty? value-map))

(defn map-remove-nil
  "Removes all top-level entries with nil values from the map.
   Uses dissoc, so reuse of map data will be maximised (.8 usec)"
  [value-map]
  (filter-remove-val nil? value-map))

(defn transform-keys
  "Transforms the top-level keys in the map using the given function.
  Uses for-map. (14.564 usec).
  Handles nil, returning nil"
  [fn value-map]
  (if (empty? value-map)
    value-map
    (pc/for-map [[key value] value-map]
                (fn key) value)))

(defn map-all-keys
  "Transforms the top-level keys in the map using the given function.
   Handles nil, returning nil"
  [map-fn value-map]
  (if (empty? value-map)
    value-map
    (pc/for-map [[key value] value-map]
                (map-fn key) (if (map? value)
                               (map-all-keys map-fn value)
                               value))))

;; ============================= Vector functions ===================

(defn vec-remove-nil
  "Removes any entries that are nil from given vector, returning a vector. eg:
```clojure
(vec-remove-nil [1 nil 3 nil 4]) => [1 3 4]
```
"
  [vec]
  (into [] (filter #(not (nil? %)) vec)))

(defn find-by-pred
  "Finds the first element in the vector that satisfies the given predicate (which accepts an element
   of the vector)"
  [vec pred]
  (->> vec
       (filter pred)
       (first)))

(defn find-element
  "Finds the element in the vector with value for the supplied key. eg:
```clojure
(find-element [{:id 1 :value 10} {:id 2 :value 100}] :id 1) => {:id 1 :value 10}
```
"
  [vec id-key id]
  (println "find-element(): vec: " vec " id-key: " id-key " id: " id)
  (find-by-pred vec #(= id (get % id-key))))

(defn replace-element-by-pred
  "Replaces a map element of a vector, given a new one and a predicate fn that must return true on the elememt to be replaced.
   Will only replace the first element matched."
  [vec replacement-element pred]
  (let [index (find-index-by-pred vec pred)]
    (if index
      (assoc vec index replacement-element)
      vec)))

(defn replace-element
  "Replaces a map element of a vector, given a new one and the keyword of the unique identifying attribute in the map. eg:
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
  "Performs a compose of functions in a collection. It will execute
   the functions in order of first to last in the collection (which is different from the clojure
   compose which executes from right to left.)
   eg:
```clojure
  ((compose-fns [inc #(Mth.abs %)]) -1)  => 0
```
"
  [coll-fns]
  (reduce comp (reverse coll-fns)))

;; ================================== String functions ==================================

(defn before
  "Returns the substring before the first occurrence of the given substring.
  eg:
```clojure
  (before \"prefix-end-next-end\" \"-end\")  =>  \"prefix\"
```
"
  [strn before-value]
  (subs strn 0 (str/index-of strn before-value)))

(defn before-last
  "Returns the substring before the last occurrence of the given substring.
  eg:
```clojure
  (before-last \"prefix-end-next-end\" \"-end\")  =>  \"prefix-end-next\"
```
"
  [strn before-value]
  (subs strn 0 (str/last-index-of strn before-value)))

;; =========================== Cross platform utility functions =========================

(defn rand-uuid
  "Platform independent uuid generator.
  java.util.UUID/randomUUID for Clojure and random-uuid for ClojureScript"
  []
  #?(:clj  (java.util.UUID/randomUUID)
     :cljs (random-uuid)))

(defn parse-int
  "Platform independent int parser.

   - Integer/parseInt for Clojure
   - js/parseInt for ClojureScript."
  [string-int]
  #?(:clj (Integer/parseInt string-int)
     :cljs (js/parseInt string-int)))

(defn current-time-millis
  "Returns the current time in millis, as appropriate for the platform"
  []
  #?(:clj (System/currentTimeMillis)
     :cljs (.getTime (js/Date.))))

(defn edn-read
  "Cross platform edn string reader"
  [str]
  #?(:clj (edn/read-string str)
     :cljs (reader/read-string str)))

;; ============================== Macros =================================


(defmacro cond-t
  "Macro for use in threading (as expr in ->).

   - val - value to be tested - should come from the threading macro
   - pred-fn - predicate function, which will be given val, or a non-function expression - which would be tested for truthy
   - map-fn - Executed on the value if the pred-fn or expression true and then returned.

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
  "Throws and IllegalArgumentException, as appropriate for the platform.

  - java.lang.IllegalArgumentException for Clojure
  - js/Error for ClojureScript with IllegalArgumentException in message"
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
