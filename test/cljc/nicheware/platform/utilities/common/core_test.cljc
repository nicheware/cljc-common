(ns nicheware.platform.utilities.common.core-test
  (:require [nicheware.platform.utilities.common.core :as sut]
            [camel-snake-kebab.core :as csk]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [nicheware.platform.utilities.common.core :as common]))

;; ================================== Sample data ==================================

(def map-with-nil
  {
   :one "one"
   :two 2
   :three nil
   :four { :nested "value" :inside nil}
   :five nil
   :size ["vector" nil]
   })

(def map-without-nil
  {
   :one "one"
   :two 2
   :four { :nested "value" :inside nil}
   :size ["vector" nil]
   })

(def map-with-odd-even
  {
   :one 1
   :two 2
   :three 3
   :four 4
   :five 5
   })

(def map-with-even
  {
   :two 2
   :four 4
   })

;; ================================= Sequence slice functions ======================

(t/deftest test-compute-start-end-count
  (t/testing "Compute all where none supplied"
    (t/is (= {:start 0 :end 2 :subset-count 3}
             (sut/compute-start-end-count [1 2 3]))))

  (t/testing "Compute where start supplied"
    (t/is (= {:start 2 :end 5 :subset-count 4}
             (sut/compute-start-end-count [1 2 3 4 5 6] {:start 2}))))

  (t/testing "Compute where end supplied"
    (t/is (= {:start 0 :end 4 :subset-count 5}
             (sut/compute-start-end-count [1 2 3 4 5 6] {:end 4}))))

  (t/testing "Compute where start and end supplied"
    (t/is (= {:start 3 :end 4 :subset-count 2}
             (sut/compute-start-end-count [1 2 3 4 5 6] {:start 3 :end 4}))))

  (t/testing "Compute where all supplied"
    (t/is (= {:start 1 :end 4 :subset-count 3}
             (sut/compute-start-end-count [1 2 3 4 5 6 7] {:start 1 :end 4 :subset-count 3})))))


(t/deftest test-slice
  (let [columns [[0 1] [1 2] [2 3] [3 4] [4 5] [5 6]]]
    (t/testing "Testing with start and end defined in a map"
      (t/is (= (list [1 2])
               (sut/slice columns {:start 1 :end 1})))
      (t/is (= (list [1 2] [2 3] [3 4])
               (sut/slice columns {:start 1 :end 3}))))

    (t/testing "Testing with start and end defined as vector"
      (t/is (= (list [1 2])
               (sut/slice columns [1 1])))
      (t/is (= (list [1 2] [2 3] [3 4])
               (sut/slice columns [1 3]))))

    (t/testing "Testing with start and end defined as params"
      (t/is (= (list [1 2])
               (sut/slice columns 1 1)))
      (t/is (= (list [1 2] [2 3] [3 4])
               (sut/slice columns 1 3))))

    (t/testing "Testing with one of both start and end missing"
      (t/is (= (list [4 5] [5 6])
               (sut/slice columns {:start 4})))
      (t/is (= (list [0 1] [1 2] [2 3] [3 4])
               (sut/slice columns {:end 3}))))

    (t/testing "Testing with one of both start and end invalid"
      (t/is (nil? (sut/slice columns {:start 6})))
      (t/is (nil? (sut/slice columns {:start 4 :end 2})))
      (t/is (nil? (sut/slice columns {:end -2})))
      )

    (t/testing "Testing with clamping invalid values"
      (t/is (= columns
               (sut/slice columns {:start -1})))
      (t/is (= columns
               (sut/slice columns {:end 8})))
      (t/is (= (list [4 5] [5 6])
               (sut/slice columns {:start 4 :end 8})))
      (t/is (= (list [0 1] [1 2] [2 3] [3 4])
               (sut/slice columns {:start -2 :end 3}))))))


(t/deftest test-slice-wrap
  (t/testing "Test that we can wrap around a slice"
    (t/is (= [5 6 0 1]
             (sut/slice-wrap [0 1 2 3 4 5 6] [5 1]))))

  (t/testing "Test that still works when no wrapping required"
    (t/is (= [2 3 4]
             (sut/slice-wrap [0 1 2 3 4 5 6] [2 4])))))

(t/deftest test-remove-slice-wrap
  (t/testing "Test the remove works when wrapping"
    (t/is (= [2 3 4]
             (sut/remove-slice-wrap [0 1 2 3 4 5 6] [5 1]))))

  (t/testing "Test the remove works when no need to wrap"
    (t/is (= [0 1 5 6]
             (sut/remove-slice-wrap [0 1 2 3 4 5 6] [2 4])))))



(t/deftest test-remove-slice
  (t/testing "Remove from middle"
    (t/is (= [0 1 6]
             (sut/remove-slice [0 1 2 3 4 5 6] {:start 2 :end 5}))))
  (t/testing "Remove from end"
    (t/is (= [0 1 2 3 4]
             (sut/remove-slice [0 1 2 3 4 5 6] {:start 5}))))
  (t/testing "Remove from start"
    (t/is (= [3 4 5 6]
             (sut/remove-slice [0 1 2 3 4 5 6] {:start 0 :end 2}))))
  (t/testing "Remove from start with wild start"
    (t/is (= [3 4 5 6]
             (sut/remove-slice [0 1 2 3 4 5 6] {:start -2 :end 2}))))
  (t/testing "Remove from end with wild end"
    (t/is (= [0 1 2 3 4]
             (sut/remove-slice [0 1 2 3 4 5 6] {:start 5 :end 20})))))

(t/deftest test-filter-count
  (t/testing "Test where should get exact number evenly"
    (t/is (= [1 3 5 7]
             (sut/filter-count (range 1 (inc 7)) 4))))

  (t/testing "Test entire range"
    (t/is (= [1 2 3 4 5 6 7 8 9 10]
             (sut/filter-count (range 1 (inc 10)) 10))))

  (t/testing "Test boundary of 1 and 2"
    (t/is (= [1]
             (sut/filter-count (range 1 (inc 10)) 1)))
    (t/is (= [1 10]
             (sut/filter-count (range 1 (inc 10)) 2))))

  (t/testing "Test where will not get exact number evenly"
    (t/is (= [1 6 10]
             (sut/filter-count (range 1 (inc 10)) 3)))
    (t/is (= [1 4 7 10]
             (sut/filter-count (range 1 (inc 10)) 4)))
    (t/is (= [1 4 7 8 10]
             (sut/filter-count (range 1 (inc 10)) 5)))
    (t/is (= [1 3 5 7 9 10]
             (sut/filter-count (range 1 (inc 10)) 6)))))


(t/deftest test-nth-items
  (t/testing "Test simple selection from a vector"
    (t/is (= [1 3 5 7]
             (sut/nth-items [0 1 2 3 4 5 6 7] [1 3 5 7]))))

  (t/testing "Test word selection from a list"
    (t/is (= ["second" "fifth"]
             (sut/nth-items (list "first" "second" "third" "forth" "fifth") [1 4])))))

;; ======================= Sequence insert functions ==================================


(t/deftest test-insert-before
  (let [columns [[0 1] [1 2] [2 3] [3 4] [4 5] [5 6]]
        length (count columns)]
    (t/testing "Testing insert at end"
      (t/is (= (concat columns [[2 2]])
               (sut/insert-before columns length [[2 2]])))
      (t/is (= [1 2 3 4 5 6]
               (sut/insert-before [1 2] 2 [3 4 5 6]))))

    (t/testing "Testing insert at start"
      (t/is (= (concat [[2 2]] columns)
               (sut/insert-before columns 0 [[2 2]])))
      (t/is (= [3 4 5 6 1 2]
               (sut/insert-before [1 2] 0 [3 4 5 6]))))

    (t/testing "Testing insert in middle"
      (t/is (= [[0 1] [1 2] [2 2] [2 3] [3 4] [4 5] [5 6]]
               (sut/insert-before columns 2 [[2 2]])))
      (t/is (= [1 3 4 5 6 2]
               (sut/insert-before [1 2] 1 [3 4 5 6]))))))

(t/deftest test-insert-after
  (let [columns [[0 1] [1 2] [2 3] [3 4] [4 5] [5 6]]
        length (count columns)]
    (t/testing "Testing insert at end"
      (t/is (= (concat columns [[2 2]])
               (sut/insert-after columns (dec length) [[2 2]])))
      (t/is (= [1 2 3 4 5 6]
               (sut/insert-after [1 2] 1 [3 4 5 6]))))

    (t/testing "Testing insert at start"
      (t/is (= (concat [[2 2]] columns)
               (sut/insert-after columns -1 [[2 2]])))
      (t/is (= [3 4 5 6 1 2]
               (sut/insert-after [1 2] -1 [3 4 5 6]))))

    (t/testing "Testing insert in middle"
      (t/is (= [[0 1] [1 2] [2 2] [2 3] [3 4] [4 5] [5 6]]
               (sut/insert-after columns 1 [[2 2]])))
      (t/is (= [1 3 4 5 6 2]
               (sut/insert-after [1 2] 0 [3 4 5 6]))))))

(defn unittest-replace
  [replace-fn]
  (let [collection [1 2 3 4 5 6]]
    (t/testing "Replace in the middle of the elements"
      (t/is (= [1 7 8 9 5 6]
               (replace-fn collection 1 [7 8 9]))))

    (t/testing "Replace at start of the elements"
      (t/is (= [7 8 9 1 2 3 4 5 6]
               (replace-fn collection -3 [7 8 9])))
      (t/is (= [7 8 9 3 4 5 6]
               (replace-fn collection -1 [7 8 9]))))))

(t/deftest test-replace-at
  (t/testing "Replace-at"
    (unittest-replace sut/replace-at)

    (let [collection [1 2 3 4 5 6]]
      (t/testing "Replace at end of the elements"
        (t/is (= [1 2 3 4 5 7 8 9]
                 (sut/replace-at collection 5 [7 8 9])))
        (t/is (= [1 2 3 4 5 6 7 8 9]
                 (sut/replace-at collection 7 [7 8 9])))))))

(t/deftest test-replace-at-wrap
  (t/testing "Replace with optional wrap"
    (unittest-replace sut/replace-at-wrap)

    (let [collection [1 2 3 4 5 6]]
      (t/testing "Test wrap replacement"
        (t/is (= [9 10 3 4 7 8]
                 (sut/replace-at-wrap collection 4 [7 8 9 10])))))))


;; ============================= Number functions ==============================

(t/deftest test-range-of-range
  (t/testing "Test inclusive range"
    (t/is (= (range 2 5)
             (sut/range-of-range {:start 2 :end 4}))))

  (t/testing "Test error"
    (t/is (= [] (sut/range-of-range {:start 2})))))

(t/deftest test-snap
  (t/testing "Snap negative"
    (t/is (= -1
             (sut/snap -0.5)))
    (t/is (= -1
             (sut/snap -1))))

  (t/testing "Snap positive"
    (t/is (= 1
             (sut/snap 0.5)))
    (t/is (= 1
             (sut/snap 1)))))

(t/deftest test-negate
  (t/testing "Negate positive"
    (t/is (= -1
             (sut/negate 1))))
  (t/testing "Negate negative"
    (t/is (= 1
             (sut/negate -1)))))



;; =========================== General sequence functions =============================

(t/deftest test-rotate-seq
  (let [col [1 2 3 4 5 6]]
    (t/testing "Index is within the sequence"
      (t/is (= [2 3 4 5 6 1]
               (sut/rotate-seq 1 col)))
      (t/is (= col
               (sut/rotate-seq 0 col)))
      (t/is (= [6 1 2 3 4 5]
               (sut/rotate-seq 5 col))))

    (t/testing "Index is out of sequence - returns original"
      (t/is (= col
               (sut/rotate-seq 6 col)))
      (t/is (= col
               (sut/rotate-seq -1 col))))))

(t/deftest test-seq-of
  (t/testing "Test sequence of int"
    (t/is (= [2 2 2 2 2 2 2]
             (sut/seq-of 2 7))))

  (t/testing "Test empty sequence"
    (t/is (= []
             (sut/seq-of 2 0)))))

(t/deftest test-pad-with
  (t/testing "Test padding existing collection"
    (t/is (= [1 2 3 0 0]
             (sut/pad-with [1 2 3] 0 5))))

  (t/testing "Test padding empty collection"
    (t/is (= [0 0 0 0]
             (sut/pad-with [] 0 4))))

  (t/testing "Test padding not needed"
    (t/is (= [1 2 3 4]
             (sut/pad-with [1 2 3 4] 0 4)))))

(t/deftest test-selective-merge
  (t/testing "Merge some values"
    (t/is (= [1 20 3 40 5]
             (sut/selective-merge [1 2 3 4 5] [10 20 30 40 50] [nil true false true false])))))

(t/deftest test-is-empty?
  (t/testing "Test simple empty vector"
    (t/is (sut/is-empty? [])))

  (t/testing "Test simple vector with elements"
    (t/is (not (sut/is-empty? [1 2 3]))))

  (t/testing "Test int, should not be true"
    (t/is (not (sut/is-empty? 23)))))

(t/deftest test-max-length
  (t/testing "Test length positive"
    (let [max-len (sut/max-length 3)]
      (t/is (max-len ""))
      (t/is (max-len "12"))
      (t/is (max-len "123"))
      (t/is (not (max-len "1234")))))

  (t/testing "Test length 0"
    (let [max-len (sut/max-length 0)]
      (t/is (max-len ""))
      (t/is (not (max-len "1")))
      (t/is (not (max-len "1234"))))))

(t/deftest test-min-length
  (t/testing "Test length positive"
    (let [min-len (sut/min-length 3)]
      (t/is (min-len "123"))
      (t/is (min-len "1234"))
      (t/is (not (min-len "12")))
      (t/is (not (min-len "1")))
      (t/is (not (min-len "")))))

  (t/testing "Test length 0"
    (let [min-len (sut/min-length 0)]
      (t/is (min-len ""))
      (t/is (min-len "1"))
      (t/is (min-len "1234")))))

;; =========================== Sequence pattern functions =============================

(defn- all-seqs
  "All sequences of length 0..max-len drawn from values, used as an exhaustive property-test domain."
  [values max-len]
  (mapcat (fn [len]
            (reduce (fn [acc _] (for [prefix acc value values] (conj prefix value)))
                    [[]]
                    (range len)))
          (range (inc max-len))))

(def property-domain
  (concat (all-seqs [:a :b nil] 5)
          (all-seqs [1 false] 6)
          ["" "a" "aab" "abcabc" "mississippi"]))

(defn- rebuild-from-motif
  "Reconstructs the original collection from a find-repeating-motif result."
  [{:keys [motif repeats exceptions]}]
  (reduce (fn [v {:keys [index actual]}] (assoc v index actual))
          (vec (apply concat (repeat repeats motif)))
          exceptions))

(t/deftest test-run-length-encode
  (t/testing "Runs of equal values"
    (t/is (= [[:a 2] [:b 1] [:a 1]]
             (sut/run-length-encode [:a :a :b :a])))
    (t/is (= [[1 3]]
             (sut/run-length-encode '(1 1 1)))))

  (t/testing "nil and false are values"
    (t/is (= [[nil 2] [false 1] [nil 1]]
             (sut/run-length-encode [nil nil false nil]))))

  (t/testing "Strings give chars"
    (t/is (= [[\a 2] [\b 1]]
             (sut/run-length-encode "aab"))))

  (t/testing "Empty and nil"
    (t/is (= [] (sut/run-length-encode [])))
    (t/is (= [] (sut/run-length-encode nil)))
    (t/is (vector? (sut/run-length-encode nil))))

  (t/testing "Values compared with ="
    (t/is (= [[[1 2] 2]]
             (sut/run-length-encode [[1 2] '(1 2)])))))

(t/deftest test-run-length-decode
  (t/testing "Expand pairs"
    (t/is (= [:a :a :b :a]
             (sut/run-length-decode [[:a 2] [:b 1] [:a 1]])))
    (t/is (vector? (sut/run-length-decode [[:a 2]]))))

  (t/testing "Zero counts contribute nothing"
    (t/is (= [:b]
             (sut/run-length-decode [[:a 0] [:b 1]]))))

  (t/testing "Empty and nil"
    (t/is (= [] (sut/run-length-decode [])))
    (t/is (= [] (sut/run-length-decode nil)))))

(t/deftest test-maximal-runs
  (t/testing "Runs with identity"
    (t/is (= [{:value :a :start 0 :end 1 :length 2}
              {:value :b :start 2 :end 2 :length 1}
              {:value :a :start 3 :end 3 :length 1}]
             (sut/maximal-runs [:a :a :b :a]))))

  (t/testing "Runs with a key function"
    (t/is (= [{:value true :start 0 :end 1 :length 2}
              {:value false :start 2 :end 4 :length 3}]
             (sut/maximal-runs [1 3 2 4 6] odd?)))
    (t/is (= [{:value 1 :start 0 :end 1 :length 2}
              {:value 2 :start 2 :end 2 :length 1}]
             (sut/maximal-runs [{:id 1} {:id 1} {:id 2}] :id))))

  (t/testing "Single run and single element"
    (t/is (= [{:value \x :start 0 :end 2 :length 3}]
             (sut/maximal-runs "xxx")))
    (t/is (= [{:value nil :start 0 :end 0 :length 1}]
             (sut/maximal-runs [nil]))))

  (t/testing "Empty and nil"
    (t/is (= [] (sut/maximal-runs [])))
    (t/is (= [] (sut/maximal-runs nil)))
    (t/is (= [] (sut/maximal-runs nil odd?)))
    (t/is (vector? (sut/maximal-runs nil)))))

(t/deftest test-maximal-runs-properties
  (doseq [coll property-domain]
    (let [runs  (sut/maximal-runs coll)
          label (pr-str coll)]
      (t/is (= (sut/run-length-encode coll) (mapv (juxt :value :length) runs)) label)
      (t/is (= (count coll) (reduce + 0 (map :length runs))) label)
      (t/is (every? #(= (:length %) (inc (- (:end %) (:start %)))) runs) label)
      (t/is (every? (fn [[a b]] (= (inc (:end a)) (:start b))) (partition 2 1 runs)) label)
      (t/is (or (empty? runs) (and (zero? (:start (first runs)))
                                   (= (dec (count coll)) (:end (last runs)))))
            label)
      (t/is (every? (fn [[a b]] (not= (:value a) (:value b))) (partition 2 1 runs)) label))))

(t/deftest test-run-length-round-trip
  (t/testing "decode of encode returns the original values"
    (doseq [coll property-domain]
      (t/is (= (vec coll)
               (sut/run-length-decode (sut/run-length-encode coll)))
            (pr-str coll)))))

(def bargello-motif [:w :w :w :k :w :w :w :r :r :r :r :k :d :d :d :d])

(def bargello-column (vec (apply concat (repeat 3 bargello-motif))))

(def bargello-column-with-exception (assoc bargello-column 3 :p))

(t/deftest test-find-repeating-motif
  (t/testing "Canonical example, exact"
    (t/is (= {:length 16 :repeats 3 :motif bargello-motif :exact? true :exceptions []}
             (sut/find-repeating-motif bargello-column)))
    (t/is (= {:length 16 :repeats 3 :motif bargello-motif :exact? true :exceptions []}
             (sut/find-repeating-motif bargello-column {:exception-budget 0}))))

  (t/testing "Canonical example with one exception, within budget"
    (t/is (= {:length     16
              :repeats    3
              :motif      bargello-motif
              :exact?     false
              :exceptions [{:index 3 :expected :k :actual :p}]}
             (sut/find-repeating-motif bargello-column-with-exception {:exception-budget 2}))))

  (t/testing "Canonical example with one exception, no budget gives whole sequence"
    (let [result (sut/find-repeating-motif bargello-column-with-exception {:exception-budget 0})]
      (t/is (= 48 (:length result)))
      (t/is (= 1 (:repeats result)))
      (t/is (= bargello-column-with-exception (:motif result)))
      (t/is (:exact? result))
      (t/is (= [] (:exceptions result)))))

  (t/testing "Period 1, all the same"
    (t/is (= {:length 1 :repeats 5 :motif [:a] :exact? true :exceptions []}
             (sut/find-repeating-motif [:a :a :a :a :a]))))

  (t/testing "Single value"
    (t/is (= {:length 1 :repeats 1 :motif [:a] :exact? true :exceptions []}
             (sut/find-repeating-motif [:a]))))

  (t/testing "No repeating motif"
    (t/is (= {:length 3 :repeats 1 :motif [:a :b :c] :exact? true :exceptions []}
             (sut/find-repeating-motif [:a :b :c]))))

  (t/testing "Budget allows a smaller period than the exact one"
    (t/is (= {:length 6 :repeats 1 :motif [:a :b :a :b :a :c] :exact? true :exceptions []}
             (sut/find-repeating-motif [:a :b :a :b :a :c])))
    (t/is (= {:length     2
              :repeats    3
              :motif      [:a :b]
              :exact?     false
              :exceptions [{:index 5 :expected :b :actual :c}]}
             (sut/find-repeating-motif [:a :b :a :b :a :c] {:exception-budget 1}))))

  (t/testing "Budget large enough accepts period 1"
    (t/is (= {:length     1
              :repeats    4
              :motif      [:a]
              :exact?     false
              :exceptions [{:index 1 :expected :a :actual :b}]}
             (sut/find-repeating-motif [:a :b :a :a] {:exception-budget 1}))))

  (t/testing "Tie with two blocks goes to the earliest block"
    (t/is (= {:length     2
              :repeats    2
              :motif      [:a :b]
              :exact?     false
              :exceptions [{:index 2 :expected :a :actual :x}
                           {:index 3 :expected :b :actual :y}]}
             (sut/find-repeating-motif [:a :b :x :y] {:exception-budget 2 :periods [2]})))
    (t/is (= [:x :b]
             (:motif (sut/find-repeating-motif [:x :b :a :b] {:exception-budget 1})))))

  (t/testing "Exceptions in ascending index order"
    (t/is (= [0 5 7]
             (map :index (:exceptions (sut/find-repeating-motif [:z :b :a :b :a :y :a :x :a :b]
                                                                {:exception-budget 3}))))))

  (t/testing "Explicit periods"
    (t/is (= 4
             (:length (sut/find-repeating-motif [:a :b :a :b :a :b :a :b] {:periods [4 8]}))))
    (t/is (= 2
             (:length (sut/find-repeating-motif [:a :b :a :b :a :b :a :b] {:periods [8 4 2]})))))

  (t/testing "No supplied period qualifies"
    (t/is (nil? (sut/find-repeating-motif [:a :b :a :c] {:periods [1 2]})))
    (t/is (nil? (sut/find-repeating-motif [:a :b :a :c] {:periods []}))))

  (t/testing "nil and false values"
    (t/is (= {:length 2 :repeats 2 :motif [nil false] :exact? true :exceptions []}
             (sut/find-repeating-motif [nil false nil false])))
    (t/is (= [{:index 2 :expected nil :actual :a}]
             (:exceptions (sut/find-repeating-motif [nil false :a false nil false] {:exception-budget 1})))))

  (t/testing "Strings and other seqables"
    (t/is (= {:length 3 :repeats 2 :motif [\a \b \c] :exact? true :exceptions []}
             (sut/find-repeating-motif "abcabc")))
    (t/is (= [1 2]
             (:motif (sut/find-repeating-motif '(1 2 1 2)))))
    (t/is (= [1 2]
             (:motif (sut/find-repeating-motif (take 6 (cycle [1 2]))))))
    (t/is (= 3
             (:repeats (sut/find-repeating-motif (range 3) {:exception-budget 2})))))

  (t/testing "Empty and nil"
    (t/is (nil? (sut/find-repeating-motif [])))
    (t/is (nil? (sut/find-repeating-motif nil)))
    (t/is (nil? (sut/find-repeating-motif "" {:exception-budget 3}))))

  (t/testing "Invalid options throw"
    (doseq [opts [{:exception-budget -1}
                  {:exception-budget 1.5}
                  {:exception-budget nil}
                  {:periods [3]}
                  {:periods [0]}
                  {:periods [-2]}
                  {:periods [2 5]}
                  {:periods [:a]}]]
      (t/is #?(:clj  (thrown? IllegalArgumentException (sut/find-repeating-motif [:a :b :a :b] opts))
               :cljs (thrown? js/Error (sut/find-repeating-motif [:a :b :a :b] opts)))
            (pr-str opts)))))

(t/deftest test-find-repeating-motif-properties
  (doseq [coll   (remove empty? property-domain)
          budget [0 1 2]]
    (let [{:keys [length repeats motif exact? exceptions] :as result}
          (sut/find-repeating-motif coll {:exception-budget budget})
          label (pr-str coll budget)]
      (t/is (= (count coll) (* length repeats)) label)
      (t/is (= length (count motif)) label)
      (t/is (<= (count exceptions) budget) label)
      (t/is (= exact? (empty? exceptions)) label)
      (t/is (= (vec coll) (rebuild-from-motif result)) label)
      (t/is (every? (fn [{:keys [index expected actual]}]
                      (and (= expected (nth motif (mod index length)))
                           (= actual (nth (vec coll) index))
                           (not= expected actual)))
                    exceptions)
            label))))

(t/deftest test-cycle-to-length
  (t/testing "Longer than coll, truncating the final repeat"
    (t/is (= [1 2 3 1 2 3 1] (sut/cycle-to-length [1 2 3] 7)))
    (t/is (= [1 2 3 1 2 3] (sut/cycle-to-length [1 2 3] 6))))

  (t/testing "Shorter than or equal to coll"
    (t/is (= [1 2] (sut/cycle-to-length [1 2 3] 2)))
    (t/is (= [1 2 3] (sut/cycle-to-length [1 2 3] 3))))

  (t/testing "Zero length"
    (t/is (= [] (sut/cycle-to-length [1 2 3] 0)))
    (t/is (= [] (sut/cycle-to-length [] 0)))
    (t/is (= [] (sut/cycle-to-length nil 0))))

  (t/testing "Other seqables"
    (t/is (= [\a \b \a] (sut/cycle-to-length "ab" 3)))
    (t/is (= [nil nil] (sut/cycle-to-length '(nil) 2)))
    (t/is (vector? (sut/cycle-to-length '(1 2) 3))))

  (t/testing "Invalid arguments throw"
    (doseq [[coll n] [[[1 2] -1] [[1 2] 1.5] [[1 2] nil] [[1 2] :a] [[] 1] [nil 3]]]
      (t/is #?(:clj  (thrown? IllegalArgumentException (sut/cycle-to-length coll n))
               :cljs (thrown? js/Error (sut/cycle-to-length coll n)))
            (pr-str coll n)))))

;; =========================== Sequence index functions =================================

(def test-vec [{:id 1 :val "one"} {:id 2 :val "two"} {:id 3 :val "three"}])
(def test-multiple [{:two 2 :three 3} {:one 1 :two 2} {:one 1 :three 3} {:four 4}])

(t/deftest test-find-first
  (t/testing "Test finding with multiple in collection"
    (t/is (= {:one 1 :two 2}
             (sut/find-first test-multiple :one))))

  (t/testing "Test not finding anything in  collection"
    (t/is (nil? (sut/find-first test-multiple :five)))))

(t/deftest test-find-last
  (t/testing "Test finding with multiple in collection"
    (t/is (= {:one 1 :three 3}
             (sut/find-last test-multiple :one))))

  (t/testing "Test not finding anything in  collection"
    (t/is (nil? (sut/find-last test-multiple :five)))))

(t/deftest test-find-index-by-pred
  (t/testing "Test a predicate that just check for a special key value"
    (t/is (= 2
             (sut/find-index-by-pred test-vec #(= (:id %) 3))))))

(t/deftest test-find-indexes-by-pred
  (t/testing "Find multiple indexes"
    (t/is (= [1 2]
             (sut/find-indexes-by-pred test-multiple :one))))

  (t/testing "Find no indexes"
    (t/is (= []
             (sut/find-indexes-by-pred test-multiple :five)))))

(t/deftest test-find-index
  (t/testing "Finds the index of an element in a vector using equality"
    (t/is (= 1
             (sut/find-index test-vec {:id 2 :val "two"}))))

  (t/testing "Finds the index of an element in a vector using function that just checks the key"
    (t/is (= 1
             (sut/find-index test-vec {:id 2 :val "different"} #(= (:id %1) (:id %2)))))))



(t/deftest test-find-indexes
  (t/testing "Find multiple indexes"
    (t/is (= [1 2]
             (sut/find-indexes test-multiple {:one 1 :six 6} #(= (:one %1) (:one %2))))))

  (t/testing "Find single index, using default compare"
    (t/is (= [2]
             (sut/find-indexes test-multiple {:one 1 :three 3}))))

  (t/testing "Find no index"
    (t/is (= []
             (sut/find-indexes test-multiple {:one 1 :three 5})))))

(t/deftest test-replace-leading-nils
  (t/testing "Replace with fixed value when have multiple nils and some following values"
    (t/is (= [1 1 2 3 4 nil]
             (sut/replace-leading-nils [nil nil 2 3 4 nil] 1)))))

(t/deftest test-replace-trailing-nils
  (t/testing "Replace when have leading and trailing nils"
    (t/is (= [nil 1 2 3 4 5 5]
             (sut/replace-trailing-nils [nil 1 2 3 4 nil nil] 5)))))


;; ========================== Utility map functions ============================

(def test-deep-map {:one {:two {:three 3 :four 4}}})

(t/deftest test-map-values
  (t/testing "Test increment all values"
    (t/is (= {:one 2 :two 3 :three 4}
             (sut/map-values inc {:one 1 :two 2 :three 3}))))

  (t/testing "Test empty map"
    (t/is (= {}
             (sut/map-values inc {})))))

(t/deftest test-dissoc-in
  (t/testing "Test deep dissoc"
    (t/is (= {:one {:two {:four 4}}}
             (sut/dissoc-in test-deep-map [:one :two :three]))))

  (t/testing "Test single level dissoc"
    (t/is (= {}
             (sut/dissoc-in test-deep-map [:one]))))

  (t/testing "Test no dissoc"
    (t/is (= test-deep-map
             (sut/dissoc-in test-deep-map [:one :two :ten])))))



(t/deftest test-assoc-in-thread-last
  (t/testing "Test working within threading macro"
    (t/is (= {:one 1 :a {:b {:c "test"}}}
             (->> {:one 1}
                  (sut/assoc-in-thread-last [:a :b :c] "test"))))))

(t/deftest test-deep-merge
  (t/testing "Merge a simple no-nested two maps"
    (t/is (= {:one 1 :two 4 :three 3}
             (sut/deep-merge {:one 1 :two 2 :three 3} {:two 4}))))

  (t/testing "Merge a two nested maps"
    (t/is (= {:one 1 :two 4 :nested {:three 4 :nested {:four 5 :five 5}}}
             (sut/deep-merge {:one 1 :two 2 :nested {:three 3 :nested {:four 4 :five 5}}}
                             {:two 4 :nested {:three 4 :nested {:four 5}}})))))

(defn- thrown-ex
  "Returns the exception thrown by calling f, or nil if it returns normally."
  [f]
  (try
    (f)
    nil
    (catch #?(:clj Exception :cljs :default) ex ex)))

(defn- ex-msg
  "Exception message on both platforms (ex-message is not in Clojure 1.9)."
  [ex]
  #?(:clj (.getMessage ^Throwable ex)
     :cljs (.-message ex)))

(t/deftest test-merge-disjoint
  (t/testing "Disjoint maps merge like merge"
    (t/is (= {:a 1 :b 2} (sut/merge-disjoint [{:a 1} {:b 2}])))
    (t/is (= {:a 1 :b 2 :c 3} (sut/merge-disjoint [{:a 1} nil {:b 2} {} {:c 3}])))
    (t/is (= {:a {:x 1}} (sut/merge-disjoint (list {:a {:x 1}}))))
    (t/is (= {:a 1 :b 2} (sut/merge-disjoint [{:a 1} {:b 2}] (fn [_] (throw (ex-info "unused" {})))))))

  (t/testing "nil and empty inputs"
    (t/is (= {} (sut/merge-disjoint nil)))
    (t/is (= {} (sut/merge-disjoint [])))
    (t/is (= {} (sut/merge-disjoint [nil nil])))
    (t/is (= {} (sut/merge-disjoint [{} {}]))))

  (t/testing "All colliding keys are reported in the default ex-info"
    (let [ex (thrown-ex #(sut/merge-disjoint [{:a 1 :b 2} {:b 3} {:a 4 :c 5}]))]
      (t/is (some? ex))
      (t/is (= {:colliding-keys #{:a :b}} (ex-data ex)))
      (t/is (re-find #":a" (ex-msg ex)))
      (t/is (re-find #":b" (ex-msg ex)))))

  (t/testing "Equal values still collide, and values never appear in the message"
    (let [ex (thrown-ex #(sut/merge-disjoint [{:k "secret-value"} {:k "secret-value"}]))]
      (t/is (= {:colliding-keys #{:k}} (ex-data ex)))
      (t/is (not (re-find #"secret-value" (ex-msg ex))))))

  (t/testing "nil values under the same key still collide"
    (t/is (= {:colliding-keys #{:k}}
             (ex-data (thrown-ex #(sut/merge-disjoint [{:k nil} {:k nil}]))))))

  (t/testing "Custom on-collision throwing its own exception"
    (let [seen (atom nil)
          ex (thrown-ex #(sut/merge-disjoint [{:a 1 :b 2} {:b 3} {:a 4}]
                                             (fn [ks]
                                               (reset! seen ks)
                                               (throw (ex-info "plug-in clash" {:type ::clash :keys ks})))))]
      (t/is (= #{:a :b} @seen))
      (t/is (= "plug-in clash" (ex-msg ex)))
      (t/is (= {:type ::clash :keys #{:a :b}} (ex-data ex)))))

  (t/testing "Custom on-collision returning normally still throws the default ex-info"
    (let [ex (thrown-ex #(sut/merge-disjoint [{:a 1} {:a 2}] (constantly {:a :merged-anyway})))]
      (t/is (= {:colliding-keys #{:a}} (ex-data ex)))))

  (t/testing "Non-map entries are illegal arguments"
    (t/is #?(:clj (thrown? IllegalArgumentException (sut/merge-disjoint [{:a 1} [:b 2]]))
             :cljs (thrown? js/Error (sut/merge-disjoint [{:a 1} [:b 2]]))))))




(t/deftest test-index-by
  (t/testing "Test creating new index"
    (t/is (= {1 {:id 1 :val "one"}, 2 {:id 2 :val "two"}, 3 {:id 3 :val "three"}}
             (sut/index-by test-vec :id)))))


(t/deftest test-filter-val
  (t/testing "Filter val"
    (t/is (= map-with-even (sut/filter-val even? map-with-odd-even)))))

(t/deftest test-filter-remove-val
  (t/testing "filter-remove-val"
    (t/is (= map-with-even (sut/filter-remove-val odd? map-with-odd-even)))))

(t/deftest test-remove-nil
  (t/testing "Multiple nil values"
    (t/is (= map-without-nil (sut/remove-nil map-with-nil)))
    (t/is (= map-without-nil (sut/remove-nil map-without-nil)))))


(t/deftest test-remove-empty
  (t/testing "Remove empty entries from map"
    (t/is (= {:one 1 :four [1]}
             (sut/remove-empty {:one 1 :two nil :three [] :four [1]}))))

  (t/testing "No entries to remove"
    (t/is (= {:one 1 :two [2]}
             (sut/remove-empty {:one 1 :two [2]})))))

(t/deftest test-filter-key
  (t/testing "Filter key"
    (t/is (= map-with-even (sut/filter-key #{:two :four} map-with-odd-even)))))

(t/deftest test-filter-remove-key
  (t/testing "Filter remove key"
    (t/is (= map-with-even (sut/filter-remove-key #{:one :three :five} map-with-odd-even)))))

(t/deftest test-map-keys
  (t/testing "Transform keys to snake case"
    (t/is (= { :services-name ["testme"] :custom-name {:*:0 ""}}
           (sut/map-keys csk/->kebab-case-keyword { :services_name ["testme"] :custom_name {:*:0 ""}})))
    (t/is (nil? (sut/map-keys identity nil)))))

(t/deftest test-map-all-keys
  (t/testing "Map all keys to snake case"
    (t/is (= { :services-name ["testme"] :custom-name {:aa-b ""}}
           (sut/map-all-keys csk/->kebab-case-keyword { :services_name ["testme"] :custom_name {:Aa_b ""}})))

    (t/is (= { :services-name ["testme"] :custom-name {:*-:-0 ""}}
           (sut/map-all-keys csk/->kebab-case-keyword { :services_name ["testme"] :custom_name {:*-:-0 ""}})))

    (t/is (= { :services-name ["testme"] :custom-name {:*:-0 ""}}
           (sut/map-all-keys (comp csk/->kebab-case-keyword csk/->camelCaseKeyword) { :services_name ["testme"] :custom_name {:*-:-0 ""}})))

    (t/is (nil? (sut/map-all-keys identity nil)))))


(t/deftest test-map-all-keys-recurse-sequentials
  (let [input {"a" [{"b" 1} {"c" [{"d" 2}]}]}]
    (t/testing "3 arity recursing into sequentials"
      (t/is (= {:a [{:b 1} {:c [{:d 2}]}]}
               (sut/map-all-keys keyword input {:recurse-sequentials? true}))))

    (t/testing "2 arity and default opts leave maps inside sequentials alone"
      (t/is (= {:a [{"b" 1} {"c" [{"d" 2}]}]} (sut/map-all-keys keyword input)))
      (t/is (= {:a [{"b" 1} {"c" [{"d" 2}]}]} (sut/map-all-keys keyword input {})))
      (t/is (= {:a [{"b" 1} {"c" [{"d" 2}]}]}
               (sut/map-all-keys keyword input {:recurse-sequentials? false})))))

  (t/testing "Top-level value may be a sequential or set"
    (t/is (= [{:a 1} {:b {:c 2}}]
             (sut/map-all-keys keyword [{"a" 1} {"b" {"c" 2}}] {:recurse-sequentials? true})))
    (t/is (= #{{:a 1} {:b 2}}
             (sut/map-all-keys keyword #{{"a" 1} {"b" 2}} {:recurse-sequentials? true}))))

  (t/testing "Sequential of sequential"
    (t/is (= [[{:a 1}] [[{:b [{:c 3}]}]]]
             (sut/map-all-keys keyword [[{"a" 1}] [[{"b" [{"c" 3}]}]]] {:recurse-sequentials? true}))))

  (t/testing "Collection types and order preserved"
    (let [result (sut/map-all-keys keyword
                                   {"v" [{"x" 1} {"y" 2}]
                                    "l" (list {"x" 1} {"y" 2})
                                    "s" (map identity [{"x" 1} {"y" 2}])
                                    "set" #{{"x" 1}}}
                                   {:recurse-sequentials? true})]
      (t/is (vector? (:v result)))
      (t/is (= [{:x 1} {:y 2}] (:v result)))
      (t/is (list? (:l result)))
      (t/is (= [{:x 1} {:y 2}] (:l result)))
      (t/is (list? (:s result)))
      (t/is (= [{:x 1} {:y 2}] (:s result)))
      (t/is (set? (:set result)))
      (t/is (= #{{:x 1}} (:set result)))))

  (t/testing "nil, empty and non-collection values"
    (doseq [v [nil {} [] () #{} 1 "s" :k]]
      (t/is (= v (sut/map-all-keys keyword v {:recurse-sequentials? true})) (pr-str v))))

  (t/testing "identity is a no-op"
    (doseq [v [{:a [{:b #{{:c 1}}} (list {:d [1 2]})]}
               [{"a" {"b" [{"c" nil}]}}]
               #{[{:a 1}]}
               {}]]
      (t/is (= v (sut/map-all-keys identity v {:recurse-sequentials? true})) (pr-str v)))))

(t/deftest test-dissoc-all-keys
  (t/testing "Spec example"
    (t/is (= {:value {:a 1 :xs [{:b 2} {}]} :removed {:starred 2 :modified-time 1}}
             (sut/dissoc-all-keys {:a 1 :starred true :xs [{:starred false :b 2} {:modified-time 3}]}
                                  #{:starred :modified-time}))))

  (t/testing "Nothing removed"
    (t/is (= {:value {:a 1} :removed {}} (sut/dissoc-all-keys {:a 1} [:b])))
    (t/is (= {:value {:a 1} :removed {}} (sut/dissoc-all-keys {:a 1} nil)))
    (t/is (= {:value {:a 1} :removed {}} (sut/dissoc-all-keys {:a 1} []))))

  (t/testing "nil, empty and scalar values"
    (doseq [v [nil {} [] () #{} 1 "s"]]
      (t/is (= {:value v :removed {}} (sut/dissoc-all-keys v [:a])) (pr-str v))))

  (t/testing "Does not descend into the value of a removed key"
    (t/is (= {:value {:b 1} :removed {:a 1}}
             (sut/dissoc-all-keys {:a {:a {:a 1}} :b 1} [:a]))))

  (t/testing "Sequential of sequential and sets"
    (t/is (= {:value [[{:b 1}] [[{}]] #{{:c 2}}] :removed {:a 3}}
             (sut/dissoc-all-keys [[{:a 1 :b 1}] [[{:a 2}]] #{{:a 3 :c 2}}] #{:a}))))

  (t/testing "Collection types and order preserved"
    (let [{:keys [value]} (sut/dissoc-all-keys {:v [{:x 1 :k 1} {:y 2}]
                                                :l (list {:x 1} {:y 2 :k 1})
                                                :s (map identity [{:k 0 :x 1} {:y 2}])
                                                :set #{{:x 1 :k 2}}}
                                               [:k])]
      (t/is (vector? (:v value)))
      (t/is (= [{:x 1} {:y 2}] (:v value)))
      (t/is (list? (:l value)))
      (t/is (= [{:x 1} {:y 2}] (:l value)))
      (t/is (list? (:s value)))
      (t/is (= [{:x 1} {:y 2}] (:s value)))
      (t/is (set? (:set value)))
      (t/is (= #{{:x 1}} (:set value)))))

  (t/testing "Removed keys are absent at every depth of the result, and counts match the input"
    (doseq [[v ks] [[{:a 1 :starred true :xs [{:starred false :b 2} {:modified-time 3}]} #{:starred :modified-time}]
                    [[{:k 1} [{:k 2 :j [{:k 3}]}] {:j {:k 4 :l (list {:k 5})}}] [:k]]
                    [{:k {:k 1} :m [{:k nil}]} [:k :m]]
                    [{"k" 1 :k 2} ["k"]]]]
      (let [key-set (set ks)
            has-removed-key? #(and (map? %) (some (partial contains? %) key-set))
            {:keys [value removed]} (sut/dissoc-all-keys v ks)]
        (t/is (= [] (sut/paths-where has-removed-key? value)) (pr-str v))
        (t/is (every? pos? (vals removed)))
        (t/is (every? key-set (keys removed)))))))

(t/deftest test-paths-where
  (t/testing "Spec example"
    (t/is (= [[:curves "S"] [:pattern :curve-specs 1]]
             (sut/paths-where #(and (map? %) (contains? % :versions))
                              {:curves {"S" {:versions {}}}
                               :pattern {:curve-specs [{:x 1} {:versions 1}]}}))))

  (t/testing "Root is tested and reported as []"
    (t/is (= [[]] (sut/paths-where map? {})))
    (t/is (= [[]] (sut/paths-where nil? nil)))
    (t/is (= [] (sut/paths-where string? nil)))
    (t/is (= [[] [:a]] (sut/paths-where map? {:a {}}))))

  (t/testing "Descends into values that matched, pre-order"
    (t/is (= [[] [:a] [:a :b] [:a :b 0]]
             (sut/paths-where coll? {:a {:b [{}]}}))))

  (t/testing "Sequential of sequential, lists indexed, sets not descended"
    (t/is (= [[0] [1 1] [1 2 0]]
             (sut/paths-where string? ["a" [1 "b" (list "c")] #{"d"}])))
    (t/is (= [[2]] (sut/paths-where set? ["a" [1] #{"d"}]))))

  (t/testing "Keys are not tested"
    (t/is (= [[:x]] (sut/paths-where keyword? {:a 1 :x :v}))))

  (t/testing "Every returned path satisfies pred via get-in"
    (doseq [[pred v] [[number? {:a 1 :b [2 {:c 3 :d [4 [5]]}] :e {:f 6}}]
                      [map? [{:a {:b [{}]}} [[{}]]]]
                      [nil? {:a nil :b [nil {:c nil}]}]
                      [vector? [[[]] {:x [[1]]}]]
                      [(constantly true) {:a [1 {:b 2}]}]]]
      (let [paths (sut/paths-where pred v)]
        (t/is (seq paths) (pr-str v))
        (t/is (every? #(pred (get-in v %)) paths) (pr-str v))
        (t/is (= (count paths) (count (distinct paths))))))))

;; ============================= Vector functions ===================


(t/deftest test-vec-remove-nil
  (t/testing "Remove nil entries from vector"
    (t/is (= [1 2 3]
             (sut/vec-remove-nil [nil 1 nil nil 2 3 nil]))))

  (t/testing "No nils to remove"
    (t/is (= [1 2 34]
             (sut/vec-remove-nil [1 2 34])))))


(t/deftest test-find-by-pred
  (t/testing "Find using predicate"
    (t/is (= {:one 1 :two 2}
             (sut/find-by-pred test-multiple :one))))

  (t/testing "Find nothing predicate"
    (t/is (nil?
             (sut/find-by-pred test-multiple :five)))))

(t/deftest test-find-element
  (t/testing "Find element with predicate"
    (t/is (= {:id 2 :val "two"}
             (sut/find-element test-vec :id 2))))

  (t/testing "Dont find element with  predicate"
    (t/is (nil? (sut/find-element test-vec :id 100)))))


(t/deftest test-replace-element-by-pred
  (t/testing "Test successfully replacing element"
    (t/is (= [{:id 1 :val "one"} {:id 2 :val "two"} {:id 100 :val 100}]
             (sut/replace-element-by-pred test-vec {:val 100 :id 100} #(= (:id %) 3)))))

  (t/testing "Test with no match"
    (t/is (= test-vec
             (sut/replace-element-by-pred test-vec {:val 100 :id 100} #(= (:id %) 55))))))

(t/deftest test-replace-element
  (let [test-vec [{:value 1 :id 1} {:value 2 :id 2} {:value 3 :id 3}]]
    (t/testing "Test replacing the second element in the list"
      (t/is (= [{:value 1 :id 1} {:value 40 :id 2} {:value 3 :id 3}]
               (sut/replace-element test-vec
                                    {:value 40 :id 2}
                                    :id))))

    (t/testing "Test replacing the last element in the list"
      (t/is (= [{:value 1 :id 1} {:value 2 :id 2} {:value 40 :id 3}]
               (sut/replace-element test-vec
                                    {:value 40 :id 3}
                                    :id))))))

;; ============================= Function manipulation ==================================

(t/deftest test-compose-fns
  (t/testing "Test that compose works as expected with two functions"
    (t/is (= 0
             ((sut/compose-fns [inc #(Math/abs %)]) -1)))
    (t/is (= 2
             ((sut/compose-fns [#(Math/abs %) inc]) -1))))

  (t/testing "Test that compose of an empty set or nil returns identity"
    (t/is (= -1
             ((sut/compose-fns []) -1)))
    (t/is (= -1
             ((sut/compose-fns nil) -1)))))

;; ========================== String functions =================================

(t/deftest test-after
  (t/testing "Can extract valid after string"
    (t/is (= "expected"
             (sut/after "afterexpected" "after"))))

  (t/testing "nil for invalid after string"
    (t/is (nil?
           (sut/after "expectedafterafter" "aftermissing")))))

(t/deftest test-before
  (t/testing "Can extract valid before string"
    (t/is (= "expected"
             (sut/before "expectedbeforeafter" "before"))))

  (t/testing "nil for invalid before string"
    (t/is (nil?
           (sut/before "expectedbeforeafter" "beforemissing")))))

(t/deftest test-before-last
  (t/testing "Can extract valid string"
    (t/is (= "part-after-expected-"
             (sut/before-last "part-after-expected-after" "after"))))

  (t/testing "nil for invalid before-last string"
    (t/is (nil?
           (sut/before-last "expectedafterafter" "aftermissing")))))


;; =========================== Cross platform utility functions =========================

(t/deftest test-rand-uuid
  (t/testing "Test not nil"
    (t/is (not (nil? (sut/rand-uuid)))))

  (t/testing "Test not same in subsequent calls"
    (t/is (not (= (sut/rand-uuid) (sut/rand-uuid)))))


  (t/testing "Test correct UUID type"
    (t/is (= (type (sut/rand-uuid))
             #?(:clj  java.util.UUID
                :cljs UUID)))))


(t/deftest test-parse-int
  (t/testing "Test valid string integer syntax to int"
    (t/is (= 22
             (sut/parse-int "22"))))

  (t/testing "Test invalid string integer syntax returns nil"
    (t/is (nil? (sut/parse-int "22AF")))))


(t/deftest test-current-time-millis
  (t/testing "Test time is increasing"
    (t/is (<= (sut/current-time-millis) (sut/current-time-millis))))

  (t/testing "Test time is valid int"
    (t/is (< 0 (sut/current-time-millis)))))

(t/deftest test-edn-read
  (t/testing "Test read clojure map"
    (t/is (= {:one 1 :two 2}
             (sut/edn-read "{:one 1 :two 2}")))))


(t/deftest test-cond-t
  (t/testing "Test that operates as expected in expression, for true"
    (t/is (= 6
             (-> 4
                 (sut/cond-t #(= % 4) inc)
                 inc))))

  (t/testing "Test that operates as expected in expression, for false"
    (t/is (= 5
             (-> 4
                 (sut/cond-t #(= % 3) inc)
                 inc)))))

(t/deftest test-throw-illegal-arg
  (t/testing "Test that throws correct exception"
    (t/is #?(:clj
             (thrown? IllegalArgumentException (sut/throw-illegal-arg "Test error"))
             :cljs
             (thrown? js/Error (sut/throw-illegal-arg "Test error")))))

  (t/testing "Test that message correct"
    (t/is (= "Test error"
             (try
               (sut/throw-illegal-arg "Test error")
               #?(:clj (catch Exception ex (.getMessage ex))
                  :cljs (catch js/Error ex (common/after (ex-message ex) "IllegalArgumentException: "))))))))

;; ============ Scratch ===============

(comment
  (defn get-num [size step]
    (int (Math/ceil (/ size step))))

  (defn check-num [size step]
    (count  (take-nth step (range size))))

  (defn get-step [n size]
    (let [step (int  (Math/ceil (/ size n)))]
      (println "get-step(): n: " n " size: " size " step: " step)
      (if (= (get-num size step) n)
        step
        (if (= (get-num size (inc step)) n)
          (inc step)
          (dec step)))))

  (defn g-step [n size]
    (let [step (int  (Math/ceil (/ size n)))]
      (println "g-step(): n: " n " size: " size " step: " step " check-num:" (check-num size step))
      (= (check-num size step) n)))

  (defn test-num
    [size step]
    (= (get-num size step) (check-num size step)))

  (g-step 4 9)

  (test-num 9 3)
  ;; => 3

  (get-num 20 2)
  ;; => 10

  (get-num 20 3)
  ;; => 7

  (get-step 7 20)
  ;; => 3

  (get-step 3 10)
  ;; => 4

  (get-step 2 20))
