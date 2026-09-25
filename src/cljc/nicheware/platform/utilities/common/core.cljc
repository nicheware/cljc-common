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
|sequence patterns| [[maximal-runs]], [[run-length-encode]], [[run-length-decode]], [[find-repeating-motif]], [[cycle-to-length]]|
|collection searching| [[find-first]], [[find-last]], [[find-index]], [[find-last-index]], [[find-nth]], [[replace-leading-nils]]|
|map utility|  [[deep-merge]], [[merge-disjoint]], [[dissoc-in]], [[index-by]]|
|map transform| [[map-values]], [[map-keys]], [[map-all-keys]]|
|map filtering| [[filter-val]], [[filter-key]], [[filter-remove-val]], [[remove-nil]], [[remove-empty]]|
|nested data| [[dissoc-all-keys]], [[paths-where]]|
|vector functions| [[vec-remove-nil]], [[find-by-pred]], [[find-element]], [[replace-element-by-pred]], [[replace-element]]|
|string functions| [[after]], [[before]], [[before-last]], [[str-to-int]]|
|cross-platform functions| [[rand-uuid]], [[parse-int]], [[current-time-millis]], [[edn-read]]|
|canonical printing| [[canonical-edn-str]]|
|slugs & digests| [[slugify]], [[truncate-on-boundary]], [[sha256-hex]] (Clojure only)|
|function composition| [[compose-fns]]|
|exceptions| [[throw-illegal-arg]]|
|threading macros| [[cond-t]]|
|debugging| [[debug]] (compiled out unless enabled)|

"
  (:require [clojure.string :as str]
            #?(:clj [clojure.edn :as edn]
               :cljs [cljs.reader :as reader]))
  #?(:cljs (:require-macros [nicheware.platform.utilities.common.core :refer [debug]])))

;; ============================== Debug macro =================================

(def ^:no-doc debug-env-var
  "Environment variable that must be \"true\" (at compile time) for any [[debug]] output."
  "COMMON_DEBUG")

(def ^:no-doc debug-config-env-var
  "Environment variable naming an optional EDN debug config file, eg {:debug-on [\"com.acme.*\"]}"
  "COMMON_DEBUG_CONFIG")

(defn ^:no-doc last-namespaces
  "Returns the last n dot separated segments of the given namespace name.

  - ns-name: namespace name as a symbol or string
  - n: number of trailing segments to keep
  - returns: string of the last n segments, eg (last-namespaces 'a.b.c.d 2) => \"c.d\""
  [ns-name n]
  (let [segments (str/split (str ns-name) #"\.")]
    (str/join "." (take-last n segments))))

(defn ^:no-doc debug-pattern
  "Normalises one :debug-on entry (string or symbol) to its pattern string, removing any
   wrapping single quotes, so both \"com.acme.*\" and 'com.acme.*' (as in a shell-style config) work."
  [entry]
  (let [s (str/trim (str entry))]
    (if (and (> (count s) 1) (str/starts-with? s "'") (str/ends-with? s "'"))
      (subs s 1 (dec (count s)))
      s)))

(defn ^:no-doc ns-matches-pattern?
  "True if the namespace name matches the regex pattern, anchored at the start of the name.
   So com.acme.stratum* matches com.acme.stratum and com.acme.stratum.core; add a trailing $ for an exact match."
  [ns-name pattern]
  (boolean (re-find (re-pattern (str "^(?:" (debug-pattern pattern) ")")) (str ns-name))))

(defn ^:no-doc debug-config-matches?
  "True if the namespace matches any pattern in the :debug-on list of the debug config map."
  [config ns-name]
  (boolean (some #(ns-matches-pattern? ns-name %) (:debug-on config))))

(defn ^:no-doc debug-on?
  "Pure decision of whether debug is on for a namespace.

  - env-enabled?: true if COMMON_DEBUG is \"true\". When false debug is always off.
  - config: the parsed debug config map, or nil when there is none. A matching :debug-on entry turns
    debug on regardless of namespace metadata.
  - ns-name: namespace being compiled.
  - ns-meta: that namespace's metadata. {:debug true} turns debug on.
  - returns: boolean"
  [{:keys [env-enabled? config ns-name ns-meta]}]
  (boolean
   (and env-enabled?
        (or (debug-config-matches? config ns-name)
            (true? (:debug ns-meta))))))

(defn ^:no-doc debug-thread-name
  "Name of the current thread; ClojureScript has only the one, reported as \"main\"."
  []
  #?(:clj  (.getName (Thread/currentThread))
     :cljs "main"))

(defn ^:no-doc debug-timestamp
  "Current local date-time string, eg \"Tue Sep 12 12:49:08 AEST 2023\" (JavaScript's Date format in ClojureScript)."
  []
  #?(:clj  (str (java.util.Date.))
     :cljs (str (js/Date.))))

(defn ^:no-doc debug-line
  "Formats one debug line.

  - thread-name, timestamp: strings
  - ns-label: shortened namespace, eg \"common.core\"
  - fn-name: function name string
  - arg-names: source form of each argument, as strings. nil for a string literal argument.
  - arg-values: evaluated value of each argument, printed with pr-str. A value with a nil
    arg-name (a string literal) is output as is, as a message.
  - returns: \"debug: (<thread>) [<timestamp>]: <ns-label>/<fn-name>(): <name>: <value>, ...\""
  [thread-name timestamp ns-label fn-name arg-names arg-values]
  (str "debug: (" thread-name ") [" timestamp "]: " ns-label "/" fn-name "(): "
       (str/join ", " (map (fn [arg-name value]
                             (if (nil? arg-name)
                               (str value)
                               (str arg-name ": " (pr-str value))))
                           arg-names arg-values))))

(defn ^:no-doc debug-print
  "Runtime half of [[debug]]: prints the debug line for the given argument names and values, returning nil."
  [ns-label fn-name arg-names arg-values]
  (println (debug-line (debug-thread-name) (debug-timestamp) ns-label fn-name arg-names arg-values)))

#?(:clj
   (defn ^:no-doc debug-settings
     "Reads the debug environment variables. Separated out so tests can redefine it.

     - returns: {:env-enabled? <boolean> :config-path <string or nil>}"
     []
     {:env-enabled? (= "true" (some-> (System/getenv debug-env-var) str/trim str/lower-case))
      :config-path  (some-> (System/getenv debug-config-env-var) str/trim not-empty)}))

#?(:clj
   (def ^:no-doc debug-config-cache
     "Parsed debug config files, as {<path> {:modified <millis> :config <map or nil>}}, re-read only on change."
     (atom {})))

#?(:clj
   (defn ^:no-doc read-debug-config
     "Reads and parses the EDN debug config file, expanding a leading ~ to the user's home directory.
      Results are cached by file modification time, as this runs for every [[debug]] macro expansion.
      A missing or unreadable file prints one warning to *err* and is treated as no config (nil)."
     [path]
     (let [file     (java.io.File. ^String (str/replace-first path #"^~(?=/|$)"
                                                              (System/getProperty "user.home")))
           modified (.lastModified file)
           cached   (get @debug-config-cache path)]
       (if (and cached (= modified (:modified cached)))
         (:config cached)
         (let [config (try
                        (let [value (edn/read-string (slurp file))]
                          (when-not (map? value)
                            (throw (ex-info "debug config must be an EDN map" {:value value})))
                          value)
                        (catch Exception e
                          (binding [*out* *err*]
                            (println (str "debug: ignoring debug config file " (.getPath file)
                                          " (" debug-config-env-var "): " (.getMessage e))))
                          nil))]
           (swap! debug-config-cache assoc path {:modified modified :config config})
           config)))))

#?(:clj
   (defn ^:no-doc debug-enabled?
     "Compile time check of whether [[debug]] output is on for the namespace, from the environment
      variables, debug config file and the namespace metadata."
     [ns-name ns-meta]
     (let [{:keys [env-enabled? config-path]} (debug-settings)]
       (and env-enabled?
            (debug-on? {:env-enabled? env-enabled?
                        :config       (some-> config-path read-debug-config)
                        :ns-name      ns-name
                        :ns-meta      ns-meta})))))

#?(:clj
   (defmacro debug
     "Prints a debug line naming the function and the name and value of each argument, eg

   ```
     debug: (nREPL-worker-3) [Tue Sep 12 12:49:08 AEST 2023]: common.core/slice(): coll: [1 2 3], range: {:start 1}
   ```

     - fn-name: name of the enclosing function, as a string.
     - args: any expressions or variables in scope. Each is labelled with its source form and printed with pr-str,
       except a string literal, which is output unlabelled as a message.
     - returns: nil

     The on/off decision is made when the macro is expanded (compiled), not at run time. When debug is off
     the call expands to nil, so the arguments are not evaluated and there is no runtime cost.

     Debug is on only when the environment variable COMMON_DEBUG is \"true\" and either:

     - the namespace metadata has {:debug true}, eg (ns ^{:debug true} my.app.core ...) or (ns my.app.core {:debug true} ...).
       A missing value or {:debug false} is off.
     - COMMON_DEBUG_CONFIG names an EDN file (a leading ~ is expanded) whose :debug-on list has a regex
       matching the start of the namespace name. A match turns debug on even when the metadata has {:debug false}.

   ```
     export COMMON_DEBUG='true'
     export COMMON_DEBUG_CONFIG='~/debug-config.edn'

     ;; ~/debug-config.edn
     {:debug-on [\"com.transacumen.stratum.*\"
                 \"com.transacumen.commons$\"]}
   ```

     Changing either variable, the config file or the metadata only takes effect when the namespace is recompiled.
     In ClojureScript the check is made by the (JVM) compiler, and the thread is always reported as \"main\".

   ```clojure
     (defn slice
       [coll range]
       (debug \"slice\" coll range (count coll))
       ...)
   ```
   "
     [fn-name & args]
     (let [cljs-ns (:ns &env)
           ns-sym  (if cljs-ns (:name cljs-ns) (ns-name *ns*))
           ns-meta (if cljs-ns
                     (merge (meta (:name cljs-ns)) (:meta cljs-ns))
                     (meta *ns*))]
       (when (debug-enabled? ns-sym ns-meta)
         `(debug-print ~(last-namespaces ns-sym 2) ~fn-name ~(mapv #(when-not (string? %) (pr-str %)) args) [~@args])))))


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
     (debug "compute-start-end-count" start-end subset)
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
   (debug "slice-wrap" range)
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
   (debug "remove-slice-wrap" range)
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


;; =========================== Sequence pattern functions =============================

(declare throw-illegal-arg)

(defn maximal-runs
  "Finds the maximal runs of consecutive elements whose `(f x)` values are equal (`=`), with their positions.

  - coll: Any seqable (vector, list, string, ...). Strings give chars.
  - f: Optional function applied to each element before comparison. Default `identity`.
  - returns: Vector of maps, one per maximal run, in order:
    - :value - `(f x)` shared by every element of the run
    - :start - 0-based index of the first element of the run
    - :end - 0-based index of the last element of the run (inclusive)
    - :length - number of elements in the run
    Empty vector for nil or an empty collection.

eg:
```clojure
(maximal-runs [:a :a :b :a])
  => [{:value :a :start 0 :end 1 :length 2}
      {:value :b :start 2 :end 2 :length 1}
      {:value :a :start 3 :end 3 :length 1}]

(maximal-runs [1 3 2 4 6] odd?)
  => [{:value true :start 0 :end 1 :length 2}
      {:value false :start 2 :end 4 :length 3}]
```
"
  ([coll]
   (maximal-runs coll identity))

  ([coll f]
   (let [add-run (fn [[runs start] run]
                   (let [length (count run)]
                     [(conj runs {:value  (f (first run))
                                  :start  start
                                  :end    (+ start length -1)
                                  :length length})
                      (+ start length)]))]
     (first (reduce add-run [[] 0] (partition-by f coll))))))

(defn run-length-encode
  "Encodes a sequence as the runs of equal consecutive values it contains.

  - coll: Any seqable (vector, list, string, ...). Values are compared with `=`. Strings give chars.
  - returns: Vector of `[value count]` pairs, one per maximal run of equal consecutive values, in order.
    Empty vector for nil or an empty collection. See [[maximal-runs]] for runs with their positions.

eg:
```clojure
(run-length-encode [:a :a :b :a]) => [[:a 2] [:b 1] [:a 1]]
(run-length-encode \"aab\") => [[\\a 2] [\\b 1]]
(run-length-encode nil) => []
```
"
  [coll]
  (mapv (juxt :value :length) (maximal-runs coll)))

(defn run-length-decode
  "Expands `[value count]` pairs back into the sequence they describe. The inverse of [[run-length-encode]].

  - pairs: Seq of `[value count]` pairs. A pair with a count of zero or less contributes nothing.
  - returns: Vector with each value repeated count times, in order. Empty vector for nil or empty pairs.

eg:
```clojure
(run-length-decode [[:a 2] [:b 1] [:a 1]]) => [:a :a :b :a]
(run-length-decode (run-length-encode coll)) => (vec coll)
```
"
  [pairs]
  (into [] (mapcat (fn [[value n]] (repeat n value))) pairs))

(defn ^:no-doc divisors
  "Returns all positive divisors of the positive integer n, in ascending order."
  [n]
  (filterv #(zero? (mod n %)) (range 1 (inc n))))

(defn ^:no-doc positional-majority
  "Returns the most frequent value in values. Ties go to the tied value that occurs first in values.
   Values may include nil or false."
  [values]
  (let [freqs (frequencies values)
        top   (apply max (vals freqs))]
    (first (filter #(= top (get freqs %)) values))))

(defn ^:no-doc period-motif
  "Folds v into consecutive blocks of length period and returns the vector of positional majority values."
  [v period]
  (mapv (fn [position] (positional-majority (take-nth period (drop position v))))
        (range period)))

(defn ^:no-doc motif-exceptions
  "Returns a vector of `{:index :expected :actual}` for each index of v whose value differs from
   the motif value at its position (index mod motif length), in ascending index order."
  [v motif]
  (let [period (count motif)]
    (into []
          (keep-indexed (fn [index actual]
                          (let [expected (nth motif (mod index period))]
                            (when (not= expected actual)
                              {:index index :expected expected :actual actual}))))
          v)))

(defn ^:no-doc motif-candidate
  "Builds the find-repeating-motif result map for v folded at the given period."
  [v period]
  (let [motif      (period-motif v period)
        exceptions (motif-exceptions v motif)]
    {:length     period
     :repeats    (quot (count v) period)
     :motif      motif
     :exact?     (empty? exceptions)
     :exceptions exceptions}))

(defn ^:no-doc validate-motif-opts
  "Throws an illegal argument exception if exception-budget is not a non-negative integer, or if any
   period is not a positive integer divisor of n."
  [n {:keys [exception-budget periods]}]
  (when-not (and (integer? exception-budget) (not (neg? exception-budget)))
    (throw-illegal-arg (str "find-repeating-motif :exception-budget must be a non-negative integer, got: "
                            (pr-str exception-budget))))
  (doseq [period periods]
    (when-not (and (integer? period) (pos? period) (zero? (mod n period)))
      (throw-illegal-arg (str "find-repeating-motif :periods must be positive divisors of the collection count "
                              n ", got: " (pr-str period))))))

(defn find-repeating-motif
  "Finds the shortest motif which, repeated end to end, reproduces the collection with at most a
   given number of deviating values.

  For each candidate period L (ascending), the collection is folded into `(count coll)/L` consecutive
  blocks of L. The candidate motif takes the majority value at each of the L positions (a tie goes to the
  value found in the earliest block at that position). Every index whose value differs from the motif
  value at its position is an exception. The first (smallest) L with no more exceptions than the budget
  is accepted.

  With the default periods, L = `(count coll)` always qualifies with no exceptions, so a collection with
  no repeating motif is reported as a single repeat of itself.

  - coll: Any seqable (vector, list, string, ...). Values compared with `=`.
  - opts: Optional map of:
    - :exception-budget - non-negative integer, maximum number of exceptions accepted. Default 0, exact repeats only.
    - :periods - seq of candidate period lengths, tried in ascending order. Each must be a positive divisor of
      `(count coll)`. Default every divisor of `(count coll)`.
  - returns: nil for nil or empty coll, or when no supplied period qualifies. Otherwise a map of:
    - :length - motif length L
    - :repeats - number of times the motif repeats, `(count coll)/L`
    - :motif - vector of the L motif values
    - :exact? - true when there are no exceptions
    - :exceptions - vector of `{:index i :expected motif-value :actual coll-value}`, 0-based index in coll, ascending
  - throws: illegal argument exception if :exception-budget is not a non-negative integer, or a period is not a
    positive divisor of `(count coll)`.

eg:
```clojure
(find-repeating-motif [:a :b :a :b :a :b])
  => {:length 2 :repeats 3 :motif [:a :b] :exact? true :exceptions []}

(find-repeating-motif [:a :b :a :b :a :c] {:exception-budget 1})
  => {:length 2 :repeats 3 :motif [:a :b] :exact? false
      :exceptions [{:index 5 :expected :b :actual :c}]}

(find-repeating-motif [:a :b :c])
  => {:length 3 :repeats 1 :motif [:a :b :c] :exact? true :exceptions []}

(find-repeating-motif [:a :b :a :c] {:periods [2]}) => nil
```
"
  ([coll]
   (find-repeating-motif coll {}))

  ([coll opts]
   (let [v    (vec coll)
         n    (count v)
         opts (merge {:exception-budget 0} opts)
         {:keys [exception-budget periods]} opts]
     (validate-motif-opts n opts)
     (when (pos? n)
       (let [candidates (if (some? periods) (sort (distinct periods)) (divisors n))]
         (some (fn [period]
                 (let [candidate (motif-candidate v period)]
                   (when (<= (count (:exceptions candidate)) exception-budget)
                     candidate)))
               candidates))))))


(defn cycle-to-length
  "Repeats the collection end to end until it is exactly n long, truncating the final repeat as needed.

  - coll: Any seqable (vector, list, string, ...) providing the values to repeat.
  - n: Non-negative integer, the exact length of the result.
  - returns: Vector of exactly n values, `(vec (take n (cycle coll)))`. Empty vector when n is 0, even for an empty coll.
  - throws: illegal argument exception if n is negative or not an integer, or if coll is empty and n is positive
    (it cannot be made n long).

eg:
```clojure
(cycle-to-length [1 2 3] 7) => [1 2 3 1 2 3 1]
(cycle-to-length [1 2 3] 2) => [1 2]
(cycle-to-length [] 0) => []
```
"
  [coll n]
  (when-not (and (integer? n) (not (neg? n)))
    (throw-illegal-arg (str "cycle-to-length n must be a non-negative integer, got: " (pr-str n))))
  (when (and (pos? n) (empty? coll))
    (throw-illegal-arg (str "cycle-to-length cannot make an empty collection " n " long")))
  (vec (take n (cycle coll))))

;; =========================== Sequence index functions =================================

(defn find-first
  "Find first value that satisfies predicate.

  - coll: Collection to be searched.
  - pred: Predicate function accepting a value from coll, returning true if matches required condition.
  - returns: First element from coll that matches the predicate. nil if nothing found.

"
  [coll pred]
  (debug "find-first" coll)
  (first (filter pred coll)))

(defn find-last
  "Find last value that satisfies predicate.

  - coll: Collection to be searched.
  - pred: Predicate function accepting a value from coll, returning true if matches required condition.
  - returns: Last element from coll that matches the predicate. Will examine all elements in coll. nil if no match.

"
  [coll pred]
  (debug "find-last" coll)
  (last (filter pred coll)))

(defn find-indexes-by-pred
  "Find all the indexes of the elements in the collection matching the given predicate.

  - coll: Collection to be searched.
  - pred: Predicate function accepting a value from coll, returning true if matches required condition.
  - returns: Lazy sequence of indexes of elements from coll that match the predicate. Empty sequence if no match.

"
  [coll pred]
  (debug "find-indexes-by-pred" coll)
  (keep-indexed #(if (pred %2) %1) coll))

(defn find-index-by-pred
  "Find the first index of the element in the collection, using the given predicate.

  - coll: Collection to be searched.
  - pred: Predicate function accepting a value from coll, returning true if matches required condition.
  - returns: Index of first element from coll that matches the predicate. nil if no match.


"
  [coll pred]
  (debug "find-index-by-pred" coll)
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
   (debug "find-index" element coll)
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
  (debug "dissoc-in" key-path)
  (if (= 1 (count key-path))
    (dissoc hmap (first key-path))
    (let [updated (update-in hmap (butlast key-path) dissoc (last key-path))]
      (debug "dissoc-in" key-path (butlast key-path))
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

(defn ^:no-doc colliding-keys
  "Returns the set of keys that occur in more than one of the given maps."
  [maps]
  (->> maps
       (mapcat keys)
       frequencies
       (keep (fn [[k n]] (when (> n 1) k)))
       set))

(defn ^:no-doc default-collision-ex
  "Builds the default ex-info thrown by [[merge-disjoint]]. Only the keys are included, never the values."
  [colliding]
  (ex-info (str "merge-disjoint: keys occur in more than one map: "
                (pr-str (sort-by pr-str colliding)))
           {:colliding-keys colliding}))

(defn merge-disjoint
  "Merges a sequence of maps like `merge`, but only when no key occurs in more than one map.

   Guards against silent \"last one wins\" merging, eg when combining registries contributed by
   independent plug-ins. A key present in two maps is a collision even when both values are equal.
   All colliding keys across all the maps are collected before failing, so the error names every one.

   - maps: Sequence of maps to merge. nil entries are ignored; any other non-map entry is an illegal argument.
   - on-collision: Optional function called with the set of colliding keys. It is expected to throw the
     caller's own exception. If it returns normally, the default ex-info is thrown anyway: a merged map is
     never returned when keys collide.
   - returns: New map containing every entry of every map. `{}` for nil or an empty sequence.
   - throws: When keys collide (and on-collision does not throw), ex-info with a message naming the keys
     and data `{:colliding-keys #{...}}`. Values are never included in the exception.

eg:
```clojure
(merge-disjoint [{:a 1} nil {:b 2}]) => {:a 1 :b 2}
(merge-disjoint [])                  => {}

(merge-disjoint [{:a 1 :b 2} {:b 3} {:a 4 :c 5}])
=> throws ex-info, (ex-data ex) => {:colliding-keys #{:a :b}}

(merge-disjoint [{:a 1} {:a 1}]
                (fn [ks] (throw (ex-info \"Duplicate plug-in keys\" {:keys ks}))))
=> throws the caller's ex-info
```
"
  ([maps] (merge-disjoint maps nil))
  ([maps on-collision]
   (let [present (remove nil? maps)]
     (when-not (every? map? present)
       (throw-illegal-arg "merge-disjoint: every non-nil entry must be a map"))
     (let [colliding (colliding-keys present)]
       (when (seq colliding)
         (when on-collision
           (on-collision colliding))
         (throw (default-collision-ex colliding)))
       (apply merge {} present)))))

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

  - map-fn: Function used to transform the keys. Takes a single argument, the existing key, returning a new key value.
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

(defn ^:no-doc rebuild-like
  "Builds a collection of the same kind as the sequential or set `coll` from the elements `xs`:
   vectors stay vectors, sets stay sets (same comparator for sorted sets), any other sequential
   (list, seq, lazy seq) becomes a list in the same order. Any other `coll` is returned unchanged."
  [coll xs]
  (cond
    (vector? coll)     (vec xs)
    (set? coll)        (into (empty coll) xs)
    (sequential? coll) (apply list xs)
    :else              coll))

(defn ^:no-doc map-all-keys-deep
  "Transforms keys of maps at any depth, including maps inside sequentials and sets."
  [map-fn value]
  (cond
    (map? value) (reduce-kv (fn [modified-map k v]
                              (assoc modified-map (map-fn k) (map-all-keys-deep map-fn v)))
                            {} value)
    (or (sequential? value)
        (set? value)) (rebuild-like value (map #(map-all-keys-deep map-fn %) value))
    :else value))

(defn map-all-keys
  "Transforms the keys in the map and nested maps using the given function.

  - map-fn: Function used to transform the keys. Takes a single argument, the existing key,  returning a new key value.
  - value-map: 2 arity argument. Map to be transformed. Only maps nested directly as map values are
    transformed; maps inside vectors, lists or sets are left alone.
  - value: 3 arity argument. Value to be transformed. With `:recurse-sequentials? true` it may be a map,
    sequential, set or any other value.
  - opts: 3 arity argument, options map:
      - `:recurse-sequentials?` (default false): when true, keys are transformed in maps at any depth,
        including maps inside sequential collections and sets. Collection types and order are preserved:
        vectors stay vectors, lists and seqs become lists in the same order, sets stay sets (elements whose
        transformed forms become equal will merge). Non-collection values are returned unchanged.
        When false, identical to the 2 arity.
  - returns: New map (or collection), with all keys, including keys in nested maps, transformed by map-fn.
    Handles nil, returning nil

eg:
```clojure
(map-all-keys keyword {\"a\" {\"b\" 1} \"c\" [{\"d\" 2}]})
=> {:a {:b 1} :c [{\"d\" 2}]}

(map-all-keys keyword {\"a\" [{\"b\" 1} {\"c\" [{\"d\" 2}]}]} {:recurse-sequentials? true})
=> {:a [{:b 1} {:c [{:d 2}]}]}

(map-all-keys keyword '({\"a\" 1} #{{\"b\" 2}}) {:recurse-sequentials? true})
=> ({:a 1} #{{:b 2}})
```
"
  ([map-fn value-map]
   (if (empty? value-map)
     value-map
     (reduce-kv (fn [modified-map k value]
                  (assoc modified-map (map-fn k)
                         (if (map? value)
                           (map-all-keys map-fn value)
                           value)))
                {} value-map)))
  ([map-fn value {:keys [recurse-sequentials?] :or {recurse-sequentials? false}}]
   (if recurse-sequentials?
     (map-all-keys-deep map-fn value)
     (map-all-keys map-fn value))))

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

;; ==================== Nested data functions ====================

(defn ^:no-doc strip-keys
  "Removes keys in key-set from maps at any depth of value.
   Returns `[stripped-value {<key> <count removed>}]`."
  [key-set value]
  (cond
    (map? value)
    (reduce-kv (fn [[stripped removed] k v]
                 (if (contains? key-set k)
                   [(dissoc stripped k) (update removed k (fnil inc 0))]
                   (let [[stripped-v removed-v] (strip-keys key-set v)]
                     [(assoc stripped k stripped-v) (merge-with + removed removed-v)])))
               [value {}]
               value)

    (or (sequential? value)
        (set? value))
    (let [results (map #(strip-keys key-set %) value)]
      [(rebuild-like value (map first results))
       (apply merge-with + {} (map second results))])

    :else
    [value {}]))

(defn dissoc-all-keys
  "Removes the given keys from every map at every depth of a nested value, counting what was removed.

   Descends into map values, sequential collections and sets. Collection types and order are preserved:
   vectors stay vectors, lists and seqs become lists in the same order, sets stay sets (elements that become
   equal once stripped will merge). The value of a removed key is discarded without being descended into,
   so keys nested inside it are neither removed again nor counted.

   - value: Nested data to strip. Any value; non-collections (including nil) are returned unchanged.
   - ks: Collection of keys to remove. nil or empty removes nothing.
   - returns: Map `{:value <stripped value> :removed {<key> <number of maps it was removed from>}}`.
     `:removed` only contains keys that were actually removed, so it is `{}` when nothing was removed.

eg:
```clojure
(dissoc-all-keys {:a 1 :starred true :xs [{:starred false :b 2} {:modified-time 3}]}
                 #{:starred :modified-time})
=> {:value {:a 1 :xs [{:b 2} {}]} :removed {:starred 2 :modified-time 1}}

(dissoc-all-keys {:a 1} [:b]) => {:value {:a 1} :removed {}}
```
"
  [value ks]
  (let [[stripped removed] (strip-keys (set ks) value)]
    {:value stripped :removed removed}))

(defn ^:no-doc collect-paths
  "Depth-first pre-order accumulation into acc of the paths below (and including) path where pred holds."
  [pred path value acc]
  (let [acc (if (pred value) (conj acc path) acc)
        descend (fn [acc k v] (collect-paths pred (conj path k) v acc))]
    (cond
      (map? value)        (reduce-kv descend acc value)
      (sequential? value) (reduce-kv descend acc (vec value))
      :else               acc)))

(defn paths-where
  "Finds the path of every value within nested data for which the predicate is truthy.

   A path is a vector of map keys and sequential indexes from the root, the same path vocabulary used by
   Malli error paths. For maps and vectors a returned path can be passed directly to `get-in`
   (lists and seqs are indexed too, but `get-in` does not support them).

   - pred: Function of one argument, called on the root and on every nested value.
   - value: Nested data to search. The root itself is tested and, when it matches, reported as path `[]`.
   - returns: Vector of paths (each a vector), empty when nothing matches.

   Traversal:

   - Map values are descended into with the map key as the path element. Keys themselves are not tested.
   - Sequential elements are descended into with their index as the path element.
   - Sets are not descended into (they have no stable index), though a set itself can match.
   - Descent continues below values that matched, so a match can contain further matches.
   - Order is depth-first pre-order (a value before its children), map entries in the map's own
     iteration order and sequential elements by index. Hash map iteration order is not something to
     rely on, so callers needing a deterministic order should sort the result.
   - Infinite lazy sequences are not supported.

eg:
```clojure
(paths-where #(and (map? %) (contains? % :versions))
             {:curves  {\"S\" {:versions {}}}
              :pattern {:curve-specs [{:x 1} {:versions 1}]}})
=> [[:curves \"S\"] [:pattern :curve-specs 1]]

(paths-where string? [\"a\" [1 \"b\"] #{\"c\"}]) => [[0] [1 1]]
(paths-where map? {})                         => [[]]
```
"
  [pred value]
  (collect-paths pred [] value []))

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
  (debug "find-element" coll id-key id)
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

;; =========================== Canonical printing functions =============================

(defn ^:no-doc type-description
  "Returns a short name for the type of v, for use in error messages. Never includes the value itself."
  [v]
  #?(:clj  (.getName (class v))
     :cljs (let [t (type v)]
             (or (when t (.-cljs$lang$ctorStr t))
                 (goog/typeOf v)))))

(defn ^:no-doc unsupported-value
  "Throws an illegal argument exception naming the type of an unsupported value (but not the value)."
  [reason v]
  (throw-illegal-arg (str "canonical-edn-str: " reason ": " (type-description v))))

(defn ^:no-doc zero-pad
  "Left pads the decimal representation of the non-negative integer n with zeros to width characters."
  [n width]
  (let [digits (str n)]
    (str (apply str (repeat (- width (count digits)) "0")) digits)))

(defn ^:no-doc finite-number?
  "True if the floating point number n is neither NaN nor infinite."
  [n]
  #?(:clj  (not (or (Double/isNaN n) (Double/isInfinite n)))
     :cljs (js/isFinite n)))

(defn ^:no-doc canonical-double
  "Canonical string for a finite floating point number. -0.0 is normalised to 0.0 (they are `=`).
   Uses the platform's `str`, which is locale independent: Double.toString on the JVM, Number.toString in JS."
  [d]
  (when-not (finite-number? d)
    (throw-illegal-arg "canonical-edn-str: NaN and infinite numbers are not supported"))
  (str (if (zero? d) 0.0 d)))

#?(:clj
   (defn ^:no-doc canonical-bigdec
     "Canonical string for a BigDecimal. Clojure `=` ignores scale (`(= 1.0M 1.00M)`), so trailing
      zeros are stripped and the result printed with BigDecimal.toString and an `M` suffix."
     [^java.math.BigDecimal bd]
     (str (.stripTrailingZeros bd) "M")))

(defn ^:no-doc canonical-number
  "Canonical string for any supported number. See [[canonical-edn-str]] for the rules."
  [n]
  #?(:clj  (cond
             (integer? n)                              (str n)
             (ratio? n)                                (str n)
             (decimal? n)                              (canonical-bigdec n)
             (or (instance? Double n) (instance? Float n)) (canonical-double (double n))
             :else                                     (unsupported-value "unsupported number type" n))
     :cljs (canonical-double n)))

(defn ^:no-doc utc-fields
  "Returns `[year month day hour minute second millis]` in UTC for the given inst."
  [inst]
  (let [ms (inst-ms inst)]
    #?(:clj  (let [t (.atOffset (java.time.Instant/ofEpochMilli ms) java.time.ZoneOffset/UTC)]
               [(.getYear t) (.getMonthValue t) (.getDayOfMonth t)
                (.getHour t) (.getMinute t) (.getSecond t) (quot (.getNano t) 1000000)])
       :cljs (let [d (js/Date. ms)]
               (when (js/isNaN ms)
                 (throw-illegal-arg "canonical-edn-str: invalid Date"))
               [(.getUTCFullYear d) (inc (.getUTCMonth d)) (.getUTCDate d)
                (.getUTCHours d) (.getUTCMinutes d) (.getUTCSeconds d) (.getUTCMilliseconds d)]))))

(defn ^:no-doc canonical-inst
  "Canonical `#inst` string for an instant: always UTC and millisecond precision.
   Formatted explicitly from the UTC fields rather than via any platform printer or formatter."
  [inst]
  (let [[year month day hour minute sec millis] (utc-fields inst)]
    (when-not (<= 0 year 9999)
      (throw-illegal-arg "canonical-edn-str: instant year outside 0000-9999"))
    (str "#inst \""
         (zero-pad year 4) "-" (zero-pad month 2) "-" (zero-pad day 2)
         "T" (zero-pad hour 2) ":" (zero-pad minute 2) ":" (zero-pad sec 2)
         "." (zero-pad millis 3) "Z\"")))

(declare canonical-edn*)

(defn ^:no-doc canonical-elements
  "Canonical strings for each element of coll, sorted when `sorted?` is true, joined by single spaces."
  [coll sorted?]
  (let [strs (map canonical-edn* coll)]
    (str/join " " (if sorted? (sort strs) strs))))

(defn ^:no-doc canonical-map
  "Canonical `{k v ...}` string. Entries are sorted by the canonical key string, with the canonical value
   string as a tie-break so the output never depends on iteration order."
  [m]
  (let [entries (->> m
                     (map (fn [[k v]] [(canonical-edn* k) (canonical-edn* v)]))
                     sort
                     (map (fn [[k v]] (str k " " v))))]
    (str "{" (str/join " " entries) "}")))

(defn ^:no-doc canonical-edn*
  "Recursive worker for [[canonical-edn-str]]; assumes the print vars have already been bound."
  [v]
  (cond
    (nil? v)        "nil"
    (boolean? v)    (str v)
    (string? v)     (pr-str v)
    (char? v)       (pr-str v)
    (keyword? v)    (str v)
    (symbol? v)     (str v)
    (number? v)     (canonical-number v)
    (inst? v)       (canonical-inst v)
    (uuid? v)       (str "#uuid \"" (str/lower-case (str v)) "\"")
    (record? v)     (unsupported-value "records are not supported" v)
    (map? v)        (canonical-map v)
    (set? v)        (str "#{" (canonical-elements v true) "}")
    (sequential? v) (str "[" (canonical-elements v false) "]")
    :else           (unsupported-value "unsupported type" v)))

(defn canonical-edn-str
  "Prints an EDN value as a canonical string, suitable as the input to a content digest (eg SHA-256).

   The guarantee: two values that are `=` produce the identical string, independent of hash map/set
   iteration order, metadata, JVM or locale defaults, and the `*print-...*` vars. Where values are not `=`
   the strings differ wherever practical; in particular a rational never prints as a double.

   - value: EDN data to print (see rules below).
   - returns: canonical EDN string, readable by [[edn-read]].
   - throws: IllegalArgumentException (see [[throw-illegal-arg]]) for NaN or infinite numbers, records,
     instants outside the years 0000-9999, and any unsupported type (functions, atoms, tagged literals,
     Java/JS objects, ...). The message names the type, never the value.

   Rules:

   - nil, booleans, keywords and symbols (with namespace) print as EDN. Metadata is ignored.
   - Strings and characters print as readable EDN literals with escapes.
     In ClojureScript characters are strings, so print as strings.
   - Integers of every type (Long, Integer, Short, Byte, BigInt, BigInteger) print as plain digits with no
     `N` suffix, since `(= 1 1N)`. Ratios print as `n/d`.
   - Doubles/floats must be finite. They print with the platform `str` (JVM Double.toString: `1.0`, `0.1`,
     `1.0E10`; floats are widened to double first). `-0.0` prints as `0.0`.
   - BigDecimals print with an `M` suffix, with trailing zeros stripped since `(= 1.0M 1.00M)`:
     `1.50M` => `1.5M`, `100M` => `1E+2M`.
   - Maps print as `{k1 v1 k2 v2}`, entries sorted by the canonical string of the key (plain string compare).
     Sorted maps are treated as any other map; records are rejected as they are not `=` to a plain map
     with the same entries, yet would print identically.
   - Sets print as `#{e1 e2}`, elements sorted by their canonical strings.
   - All sequential collections (vectors, lists, seqs, lazy seqs, ranges, map entries...) print as vectors
     `[e1 e2]`, since `(= [1 2] '(1 2))`. Infinite sequences never return.
   - Instants (`java.util.Date`, `java.time.Instant`; `js/Date`) print as
     `#inst \"yyyy-MM-ddTHH:mm:ss.SSSZ\"`, always UTC and truncated to millisecond precision.
     Dates use the proleptic Gregorian (ISO-8601) calendar on both platforms. Clojure's JVM `#inst` reader
     uses the Julian calendar before 1582-10-15, so such early instants do not read back as `=` on the JVM.
   - UUIDs print as `#uuid \"...\"` in lower case.

   Platform differences: ClojureScript numbers are all JS doubles, printed with Number.toString, so
   integral values have no decimal point (`1.0` prints as `1` in ClojureScript but `1.0` on the JVM), and
   there are no ratios or BigDecimals. Digests of values containing doubles are therefore platform specific.
   ClojureScript UUIDs preserve the case they were created with (and `=` is case-sensitive there), but
   are always printed lower case.

eg:
```clojure
(canonical-edn-str {:b [1 2/3 \"x\"] :a #{:z :y} nil 1})
=> \"{:a #{:y :z} :b [1 2/3 \\\"x\\\"] nil 1}\"

(canonical-edn-str '(1 1N 1.0 1.0M))                         => \"[1 1 1.0 1M]\"
(canonical-edn-str #inst \"2026-08-27T19:30:00.000+10:00\")  => \"#inst \\\"2026-08-27T09:30:00.000Z\\\"\"
(canonical-edn-str (atom 1))                                 => throws IllegalArgumentException
```
"
  [value]
  (binding [*print-readably* true
            *print-meta*     false
            *print-length*   nil
            *print-level*    nil
            #?@(:clj [*print-dup* false
                      *print-namespace-maps* false])]
    (canonical-edn* value)))

;; =========================== Slug and digest functions =============================

(defn ^:no-doc validate-separator
  "Throws unless separator is a non-empty string."
  [fn-name separator]
  (when-not (and (string? separator) (seq separator))
    (throw-illegal-arg (str fn-name ": separator must be a non-empty string"))))

(defn ^:no-doc trim-trailing-separators
  "Removes every trailing occurrence of separator from s."
  [s separator]
  (if (str/ends-with? s separator)
    (recur (subs s 0 (- (count s) (count separator))) separator)
    s))

(defn ^:no-doc trim-separators
  "Removes every leading and trailing occurrence of separator from s."
  [s separator]
  (loop [trimmed (trim-trailing-separators s separator)]
    (if (str/starts-with? trimmed separator)
      (recur (subs trimmed (count separator)))
      trimmed)))

(defn truncate-on-boundary
  "Shortens a string to at most max-length characters, preferring to cut just before a separator
   so that words are not split.

   The result is the longest prefix of s, of length <= max-length, that ends just before an occurrence of
   separator, with any trailing separators removed. If there is no such boundary (or it would leave an
   empty string), s is hard-cut to exactly max-length characters.

   - s: string to truncate. nil returns nil.
   - max-length: maximum length of the result, a non-negative integer.
   - separator: non-empty string marking the boundaries at which s may be cut.
   - returns: s unchanged if `(count s) <= max-length`, otherwise the truncated string.
   - throws: IllegalArgumentException for a non-string s, invalid max-length or invalid separator.

eg:
```clojure
(truncate-on-boundary \"bargello-bag-ocean\" 14 \"-\") => \"bargello-bag\"
(truncate-on-boundary \"bargello--bag\" 10 \"-\")     => \"bargello\"
(truncate-on-boundary \"abc\" 5 \"-\")                => \"abc\"
(truncate-on-boundary \"abcdefgh\" 4 \"-\")           => \"abcd\"
(truncate-on-boundary nil 4 \"-\")                  => nil
```
"
  [s max-length separator]
  (when-not (and (integer? max-length) (not (neg? max-length)))
    (throw-illegal-arg "truncate-on-boundary: max-length must be a non-negative integer"))
  (validate-separator "truncate-on-boundary" separator)
  (when-not (or (nil? s) (string? s))
    (throw-illegal-arg "truncate-on-boundary: s must be a string or nil"))
  (cond
    (nil? s)                  nil
    (<= (count s) max-length) s
    :else (let [boundary (str/last-index-of s separator max-length)
                prefix   (when boundary
                           (trim-trailing-separators (subs s 0 boundary) separator))]
            (if (seq prefix)
              prefix
              (subs s 0 max-length)))))

(defn ^:no-doc strip-diacritics
  "Unicode NFKD normalises s then removes all combining marks (Unicode category M)."
  [s]
  #?(:clj  (-> (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFKD)
               (str/replace #"\p{M}+" ""))
     :cljs (-> (.normalize s "NFKD")
               (.replace (js/RegExp. "\\p{M}+" "gu") ""))))

(defn ^:no-doc lower-case-root
  "Lower cases s independently of the default locale."
  [s]
  #?(:clj  (.toLowerCase ^String s java.util.Locale/ROOT)
     :cljs (.toLowerCase s)))

(defn slugify
  "Converts free text into a deterministic slug of `[a-z0-9]` runs joined by a separator, suitable for
   file names and object-store keys. The result does not depend on the default locale.

   Steps:

   1. Unicode NFKD normalisation, then all combining marks are removed (`é` => `e`).
   2. Lower case, locale independently (so `I` => `i` even in a Turkish locale).
   3. Every maximal run of characters outside `[a-z0-9]` becomes one separator, and leading and trailing
      separators are removed.
   4. If `:max-length` is given, the slug is shortened with [[truncate-on-boundary]] and re-trimmed.

   Characters with no ASCII decomposition are treated as separators, so a string with no ASCII letters or
   digits returns `\"\"`: the caller decides the fallback. Note `ß` has no NFKD decomposition, so
   `\"Straße\"` => `\"stra-e\"`.

   ClojureScript requires a JS engine supporting `String.prototype.normalize` and Unicode property escapes
   in RegExp (`\\p{M}` with the `u` flag): ES2018, eg Node 10+ and all current browsers. For the inputs
   tested the output is identical on the JVM and in ClojureScript.

   - s: string to slugify. nil returns nil.
   - opts: optional map:
     - :separator: non-empty string placed between runs, default `\"-\"`. It should not contain `[a-z0-9]`.
     - :max-length: optional non-negative integer maximum length of the slug, default nil (unlimited).
   - returns: the slug, possibly `\"\"`.
   - throws: IllegalArgumentException for a non-string s or invalid opts.

eg:
```clojure
(slugify \"Bargello Bag #3 — Ocean/Sunset\")                 => \"bargello-bag-3-ocean-sunset\"
(slugify \"  Café  Waves  \")                                => \"cafe-waves\"
(slugify \"Bargello Bag #3 — Ocean/Sunset\" {:max-length 14}) => \"bargello-bag-3\"
(slugify \"Café Waves\" {:separator \"_\"})                    => \"cafe_waves\"
(slugify \"水面\")                                           => \"\"
```
"
  ([s] (slugify s nil))
  ([s {:keys [separator max-length] :or {separator "-"}}]
   (validate-separator "slugify" separator)
   (when-not (or (nil? s) (string? s))
     (throw-illegal-arg "slugify: s must be a string or nil"))
   (when s
     (let [slug (-> s
                    strip-diacritics
                    lower-case-root
                    (str/replace #"[^a-z0-9]+" (constantly separator))
                    (trim-separators separator))]
       (if max-length
         (-> slug
             (truncate-on-boundary max-length separator)
             (trim-separators separator))
         slug)))))

#?(:clj
   (defn ^:no-doc bytes->hex
     "Lower-case hex string of a byte array, two characters per byte. Locale independent."
     [^bytes data]
     (let [digits "0123456789abcdef"]
       (->> data
            (mapcat (fn [b]
                      (let [v (bit-and (int b) 0xff)]
                        [(nth digits (bit-shift-right v 4)) (nth digits (bit-and v 0x0f))])))
            (apply str)))))

#?(:clj
   (defn sha256-hex
     "SHA-256 digest of a string or byte array, as lower-case hex. Clojure (JVM) only.

      Not provided in ClojureScript: there is no synchronous SHA-256 there, as the WebCrypto
      `crypto.subtle.digest` API is asynchronous.

      - data: String (encoded as UTF-8 before hashing) or byte array.
      - returns: 64 character lower-case hex string.
      - throws: IllegalArgumentException for any other type (including nil).

eg:
```clojure
(sha256-hex \"abc\")                   => \"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad\"
(sha256-hex (.getBytes \"abc\" \"UTF-8\")) => \"ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad\"
(sha256-hex (canonical-edn-str config)) => digest independent of map ordering
```
"
     [data]
     (let [^bytes data-bytes (cond
                               (string? data) (.getBytes ^String data java.nio.charset.StandardCharsets/UTF_8)
                               (bytes? data)  data
                               :else (throw-illegal-arg (str "sha256-hex: data must be a String or byte array, not "
                                                             (if (nil? data) "nil" (type-description data)))))]
       (-> (java.security.MessageDigest/getInstance "SHA-256")
           (.digest data-bytes)
           bytes->hex))))

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
