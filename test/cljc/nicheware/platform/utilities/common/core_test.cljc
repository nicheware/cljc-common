(ns nicheware.platform.utilities.common.core-test
  (:require [nicheware.platform.utilities.common.core :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

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

(t/deftest test-selective-merge
  (t/testing "Merge some values"
    (t/is (= [1 20 3 40 5]
             (sut/selective-merge [1 2 3 4 5] [10 20 30 40 50] [nil true false true false])))))

;; =========================== Sequence index functions =================================

(def test-vec [{:id 1 :val "one"} {:id 2 :val "two"} {:id 3 :val "three"}])

(t/deftest test-find-index-by-pred
  (t/testing "Test a predicate that just check for a special key value"
    (t/is (= 2
             (sut/find-index-by-pred test-vec #(= (:id %) 3))))))

(t/deftest test-find-index
  (t/testing "Finds the index of an element in a vector using equality"
    (t/is (= 1
             (sut/find-index test-vec {:id 2 :val "two"}))))

  (t/testing "Finds the index of an element in a vector using function that just checks the key"
    (t/is (= 1
             (sut/find-index test-vec {:id 2 :val "different"} #(= (:id %1) (:id %2)))))))


(t/deftest test-replace-leading-nils
  (t/testing "Replace with fixed value when have multiple nils and some following values"
    (t/is (= [1 1 2 3 4 nil]
             (sut/replace-leading-nils [nil nil 2 3 4 nil] 1)))))

(t/deftest test-replace-trailing-nils
  (t/testing "Replace when have leading and trailing nils"
    (t/is (= [nil 1 2 3 4 5 5]
             (sut/replace-trailing-nils [nil 1 2 3 4 nil nil] 5)))))


;; ========================== Utility map functions ============================

(t/deftest test-deep-merge
  (t/testing "Merge a simple no-nested two maps"
    (t/is (= {:one 1 :two 4 :three 3}
             (sut/deep-merge {:one 1 :two 2 :three 3} {:two 4}))))

  (t/testing "Merge a two nested maps"
    (t/is (= {:one 1 :two 4 :nested {:three 4 :nested {:four 5 :five 5}}}
             (sut/deep-merge {:one 1 :two 2 :nested {:three 3 :nested {:four 4 :five 5}}}
                             {:two 4 :nested {:three 4 :nested {:four 5}}})))))


;; ============================= Vector functions ===================

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
