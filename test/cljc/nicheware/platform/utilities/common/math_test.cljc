(ns nicheware.platform.utilities.common.math-test
  (:require [nicheware.platform.utilities.common.math :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

;; ======================== Value manipulations  ================================



(t/deftest test-roundn
  (t/testing "Test round up"
    (t/is (= 1.544
             (sut/roundn 3 1.54364))))

  (t/testing "Test round down"
    (t/is (= 1.5436
             (sut/roundn 4 1.54364)))))

(t/deftest test-round
  (t/testing "Round a float"
    (t/is (= 2
             (sut/round 1.66))))

  (t/testing "Round an int"
    (t/is (= 2
             (sut/round 2)))))

(t/deftest test-ceil
  (t/testing "Ensure returned ceiling"
    (t/is (= 2
             (sut/ceil 1.2)))))

(t/deftest test-floor
  (t/testing "Ensure returned floor"
    (t/is (= 1
             (sut/floor 1.8)))))


(t/deftest test-div
  (t/testing "Test float value returned"
    (t/is (= #?(:clj Double :cljs js/Number)
             (type (sut/div 4 3)))))

  (t/testing "Test float value correct"
    (t/is (= 0.5
             (sut/div 10 20)))))

(t/deftest test-diff
  (t/testing "Ensure absolute for negative"
    (t/is (= 1
             (sut/diff 1 2))))

  (t/testing "Ensure absolute for positive"
    (t/is (= 1
             (sut/diff 2 1)))))

(t/deftest test-mult
  (t/testing "Multiply with 4 decimal places"
    (t/is (= 5.0234
             (sut/mult 1.74 2.887)))))

(t/deftest test-clamp-change
  (t/testing "Clamp increase"
    (t/is (= 4
             (sut/clamp-change 2 6 2))))

  (t/testing "Clamp  decrease"
    (t/is (= 4
             (sut/clamp-change 6 2 2))))

  (t/testing "Clamp  working with negatives"
    (t/is (= -1
             (sut/clamp-change 1 -2 2))))

  (t/testing "Clamp not required"
    (t/is (= 6
             (sut/clamp-change 2 6 5)))))

;; ======================== Sequence generators =================================

(t/deftest test-max-abs
  (t/testing "Return max where biggest is negative"
    (t/is (= -5
             (sut/max-abs 2 -5)))
    (t/is (= -5
             (sut/max-abs -2 -5))))

  (t/testing "Return max where biggest is positive"
    (t/is (= 6
             (sut/max-abs 6 -5)))
    (t/is (= 6
             (sut/max-abs 6 4)))))

(t/deftest test-make-ratio-sequence-fn
  (t/testing "Test a halving function for 3 values between 0 and 10"
    (t/is (= [5.0 7.5 8.75]
             (map (sut/make-ratio-sequence-fn 0.5 10) [1 2 3])))))

(t/deftest test-ratio-sequence
  (t/testing "Test getting 3 values from small range"
    (t/is (= [0 5.0 7.5 8.75]
             (take 4 (sut/ratio-sequence 0.5 0 10)))))

  (t/testing "Test getting 3 values from reverse range"
    (t/is (= [10 5.0 2.5 1.25]
             (take 4 (sut/ratio-sequence 0.5 10 0))))))
