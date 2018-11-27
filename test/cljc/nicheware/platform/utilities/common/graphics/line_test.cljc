(ns nicheware.platform.utilities.common.graphics.line-test
  (:require [nicheware.platform.utilities.common.graphics.line :as sut]
            [nicheware.platform.utilities.common.math :as math]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))


;; ========================== Equations ===========================================

(t/deftest test-straight-line-equation
  (t/testing "Test simple one used in curve"
    (let [line-fn (sut/straight-line-equation [1 1] [10 4])]
      (t/is (= 1.0 (line-fn 1)))
      (t/is (= 4.0 (line-fn 10)))
      (t/is (= 2.333 (math/roundn 3 (line-fn 5))))
      (t/is (= 4 (Math/round (line-fn 9)))))))

(defn unittest-lerp
  "Utility function to allow testing of any lerp function. Means we can test make-lerp and lerp "
  [lerp-fn [x1 y1 :as start] [x2 y2 :as end]]
  (println "Lerp: " (lerp-fn 0.0))
  (t/testing "Test start of line"
    (t/is (= [(float x1) (float y1)]
             (lerp-fn 0.0))))

  (println "Lerp: " (lerp-fn 1.0))
  (t/testing "Test end of line"
    (t/is (= [(float x2) (float y2)]
             (lerp-fn 1.0))))

  (println "Lerp: " (lerp-fn 0.5))
  (t/testing "Test middle of line"
    (t/is (= [(float (/ (- x2 x1) 2)) (float (/ (- y2 y1) 2))]
             (lerp-fn 0.5)))))

(t/deftest test-make-lerp
  (t/testing "Make lerp"
    (let [start [0 0]
          end [6 6]
          lerp-fn (sut/make-lerp start end)]
      (unittest-lerp lerp-fn start end)))

  (t/testing "Test with 3 co-ords"
    (let [lerp-fn (sut/make-lerp [0 0 0] [6 8 10])]
      (t/testing "Start of co-ords"
        (t/is (= [0 0 0]
                 (lerp-fn 0))))

      (t/testing "Middle of co-ords"
        (t/is (= [3.0 4.0 5.0]
                 (lerp-fn 0.5)))))))

(t/deftest test-lerp
  (t/testing "lerp"
    (let [start [0 0]
          end [6 6]
          lerp-fn (partial sut/lerp start end)]
      (unittest-lerp lerp-fn start end)))

  (t/testing "Test with 3 co-ords"
    (let [lerp-fn (partial sut/lerp [0 0 0] [6 8 10])]
      (t/testing "Start of co-ords"
        (t/is (= [0 0 0]
                 (lerp-fn 0)))))))


(t/deftest test-bezier-quadratic-equation
  (let [bezier-fn (sut/bezier-quadratic-equation [0 0] [2 4] [6 0])
        bezier-fn-2 (sut/bezier-quadratic-equation [0 0] [3 3 ][6 0])]
    (t/testing "Test the start of the curve"
      (t/is (= [0.0 0.0]
               (bezier-fn 0.0))))
    (t/testing "Test the end of the curve"
      (t/is (= [6.0 0.0]
               (bezier-fn 1.0))))
    (t/testing "Test the middle of the curve"
      (t/is (= [2.5 2.0]
               (bezier-fn 0.5)))
      (t/is (= [3.0 1.5]
               (bezier-fn-2 0.5))))))

;; Test values obtained from: https://www3.nd.edu/~gconant/misc/bezier/
;; Shapes of curves can be displayed at: https://www.desmos.com/calculator/cahqdxeshd
(t/deftest test-bezier-cubic-equation
  (let [bezier-fn (sut/bezier-cubic-equation [0 0] [0.5 3] [5 2] [6 6])
        bezier-fn-2 (sut/bezier-cubic-equation [0 0] [1 3] [9 3] [6 6])
        round-fn (fn [point] (map #(math/roundn 2 %) point))]
    (t/testing "Test the start of the curve"
      (t/is (= [0.0 0.0]
               (bezier-fn 0.0))))
    (t/testing "Test the end of the curve"
      (t/is (= [6.0 6.0]
               (bezier-fn 1.0))))
    (t/testing "Test the middle of the curve"
      (t/is (= [1.01 1.64]
               (round-fn (bezier-fn 0.25))))
      (t/is (= [2.81 2.63]
               (round-fn (bezier-fn 0.5))))
      (t/is (= [4.71 3.8]
               (round-fn (bezier-fn 0.75))))
      (t/is (= [4.5 3.0]
               (bezier-fn-2 0.5))))))



(t/deftest test-make-curve-fn-from-samples
  (t/testing "Test a straight line, not a curve"
    (let [curve-fn (sut/make-curve-fn-from-samples [[0 0] [5 5]])]
      (t/is (= 0.0
               (curve-fn 0)))
      (t/is (= 5.0
               (curve-fn 5)))
      (t/is (= 3.0
               (curve-fn 3)))))
  (t/testing "Test 3 sample points, with sample in the middle"
    (let [curve-fn (sut/make-curve-fn-from-samples [[0 0] [3 6] [6 0]])]
      (t/is (= 0.0
               (curve-fn 0)))
      (t/is (= 0.0
               (curve-fn 6)))
      (t/is (= 6.0
               (curve-fn 3)))
      (t/is (= 3.0
               (curve-fn 1.5)))
      (t/is (= 3.0
               (curve-fn 4.5)))
      (t/is (= nil
               (curve-fn 6.5)))
      (t/is (= nil
               (curve-fn -0.5))))))


(t/deftest test-rasterize-bezier-quadratic
  (t/testing "Test points on a simple balanced curve"
    (t/is (= [[0 0] [1 1] [2 1] [3 2] [4 1] [5 1] [6 0]]
             (sut/rasterize-bezier-quadratic [0 0] [6 0] [3 3]))))

  (t/testing "Test points on a skewed small curve"
    (t/is (= [[0 0] [1 1] [2 2] [3 1] [4 1] [5 1] [6 0]]
             (sut/rasterize-bezier-quadratic [0 0] [6 0] [1 3]))))

  (t/testing "Test points on a skewed large curve"
    (t/is (= [[0 0] [1 2] [2 3] [3 3] [4 3] [5 2] [6 0]]
             (sut/rasterize-bezier-quadratic [0 0] [6 0] [2 7])))

    (t/is (= [[0 0] [1 4] [2 5] [3 5] [4 3] [5 2] [6 0]]
             (sut/rasterize-bezier-quadratic [0 0] [6 0] [1 10]))))

  (t/testing "Test points on a downward curve"
    (t/is (= [[0 6] [1 5] [2 5] [3 5] [4 5] [5 5] [6 6]]
             (sut/rasterize-bezier-quadratic [0 6] [6 6] [3 3]))))

  (t/testing "Test points on a downward curve same slope as first upward curve"
    (t/is (= [[0 3] [1 2] [2 2] [3 2] [4 2] [5 2] [6 3]]
             (sut/rasterize-bezier-quadratic [0 3] [6 3] [3 0]))))

  (t/testing "Test curve similar to Simple pattern curve"
    (t/is (= [[3 8] [4 5] [5 4] [6 3] [7 3] [8 3] [9 3] [10 3] [11 3]]
             (sut/rasterize-bezier-quadratic [3 8] [11 3] [4 1])))))


(t/deftest test-interpolate-n-points
  (t/testing "Use linear interpolation"
    (let [interpolate-fn (sut/make-lerp [0 0] [3 3])]
      (t/is (= [[1 1] [2 2]]
               (sut/interpolate-n-points interpolate-fn 2)))))

  ;; Values obtained from test above
  (t/testing "Test using cubic bezier function"
    (let [bezier-fn (sut/bezier-cubic-equation [0 0] [0.5 3] [5 2] [6 6])
          round-fn (fn [point] (map #(math/roundn 2 %) point))]
      (t/is (= [[1.01 1.64] [2.81 2.63] [4.71 3.8]]
               (map round-fn (sut/interpolate-n-points bezier-fn 3)))))))
