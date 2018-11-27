(ns nicheware.platform.utilities.common.math-test
  (:require [nicheware.platform.utilities.common.math :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

;; ======================== Value manipulations  ================================


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
