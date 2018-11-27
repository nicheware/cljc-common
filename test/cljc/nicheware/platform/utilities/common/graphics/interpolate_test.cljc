(ns nicheware.platform.utilities.common.graphics.interpolate-test
  (:require [nicheware.platform.utilities.common.graphics.interpolate :as sut]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])
            [nicheware.platform.utilities.common.math :as math]))


(t/deftest test-interpolate
  (let [round-fn (fn [point] (map #(math/roundn 2 %) point))]
    (t/testing "Test linear interpolation"
      (t/is (= [[1 1] [2 2]]
               (sut/interpolate [0 0] [3 3] 2 {:type :linear} )))

      (t/is (= [[0 1] [0 2]]
               (sut/interpolate [0 0] [3 3] 2 {:type :linear :active-dimensions [false true]} ))))

    (t/testing "Quadratic bezier interpolation"
      (t/is (= [[3.0 1.5]]
               (map round-fn (sut/interpolate [0 0] [6 0] 1 {:type :quadratic-bezier :control1 [3 3]} )))))

    ;; Data taken from tests in graphics/line_test.cljc
    (t/testing "Cubic bezier interpolation"
      (t/is (= [[1.01 1.64] [2.81 2.63] [4.71 3.8]]
               (map round-fn (sut/interpolate [0 0] [6 6] 3 {:type :cubic-bezier
                                                             :control1 [0.5 3]
                                                             :control2 [5 2]})))))

    ;; Theses ease-in and ease-out values obtained just from running the test, but
    ;; they logically make sense
    ;;
    ;; ease-in: Start with small changes getting bigger. Each changes is (point<n> - point<n-1>)
    ;;          1.32 1.44 1.56 1.68
    ;;
    ;; ease-out: Start with large changes getting smaller. Each change is
    ;;          1.68 1.56 1.44 1.32    (the reverse of ease-in as expected)
    (t/testing "Ease-in interpolation"
      (t/is (= [[1.32 1.32] [2.76 2.76] [4.32 4.32]]
               (map round-fn (sut/interpolate [0 0] [6 6] 3 {:type :ease-in}))))

      ;; The custom ease amount is slightly larger, so bigger step size
      (t/is (= [[1.39 1.39] [2.85 2.85] [4.39 4.39]]
               (map round-fn (sut/interpolate [0 0] [6 6] 3 {:type :ease-in
                                                             :ease 0.45})))))

    (t/testing "Ease-out interpolation"
      (t/is (= [[1.68 1.68] [3.24 3.24] [4.68 4.68]]
               (map round-fn (sut/interpolate [0 0] [6 6] 3 {:type :ease-out})))))

    ;; This make sense - point in the middle, and the 1st and last are the same distance from start and end.
    (t/testing "Ease-in-out interpolation"
      (t/is (= [[1.65 1.65] [3.0 3.0] [4.35 4.35]]
               (map round-fn (sut/interpolate [0 0] [6 6] 3 {:type :ease-in-out})))))

    (t/testing "Step interpolation"
      (t/is (= [[1.0 1.0] [2.0 2.0] [3.0 3.0]]
               (map round-fn (sut/interpolate [0 0] [6 6] 2 {:type :step-up
                                                             :step {:fraction 0.1 :ranges [10 10]}}))))

      (t/is (= [[5.0 5.0] [4.0 4.0] [3.0 3.0]]
               (map round-fn (sut/interpolate [6 6] [0 0] 2 {:type :step-down
                                                             :step {:fraction 0.1 :ranges [10 10]}}))))

      (t/testing " with default step value and active-dimensions"
        (t/is (= [[10.0 9.5] [10.0 9.0] [10.0 8.5]]
                 (map round-fn (sut/interpolate [10 10] [0 0] 2 {:type :step-down
                                                                 :active-dimensions [false true]}))))))))
