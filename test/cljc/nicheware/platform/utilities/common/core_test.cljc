(ns nicheware.platform.utilities.common.core-test
  (:require [nicheware.platform.utilities.common.core :as sut]
            [camel-snake-kebab.core :as csk]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

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
    (t/is (thrown? NullPointerException (sut/range-of-range {:start 2})))))

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

((t/deftest test-max-length
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
       (t/is (not (max-len "1234")))))))

((t/deftest test-min-length
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
       (t/is (min-len "1234"))))))

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
    (t/is (= (type  (sut/rand-uuid)) java.util.UUID))))


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
    (t/is (thrown? IllegalArgumentException (sut/throw-illegal-arg "Test error"))))

  (t/testing "Test that message correct"
    (t/is (= "Test error"
             (try
               (sut/throw-illegal-arg "Test error")
               (catch Exception ex (.getMessage ex)))))))

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
