(ns nicheware.platform.utilities.common.graphics-test
  (:require [nicheware.platform.utilities.common.graphics :as sut]
            [nicheware.platform.utilities.common.math :as math]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))


;; ================================= Size fitting functions =============================

(t/deftest test-aspect-ratio
  (t/testing "Test we can determine aspect ratio"
    (t/is (= 0.5
             (sut/aspect-ratio {:width 50 :height 100})))))

(t/deftest test-fit-to-height
  (t/testing "Small rect within larger rect, integer scale"
    (t/is (= {:width 20.0 :height 100 :x 40.0 :y 0 :scale 2.0}
             (sut/fit-to-height {:width 100 :height 100} {:width 10 :height 50}))))

  (t/testing "Small rect within larger rect, fraction scale"
    (t/is (= {:width 200.0004 :height 100 :x -50.0002 :y 0 :scale 16.6667}
             (sut/fit-to-height {:width 100 :height 100} {:width 12 :height 6}))))

  (t/testing "Large rect within smaller rect"
    (t/is (= {:width 10.0 :height 10 :x 0.0 :y 0 :scale 0.2}
             (sut/fit-to-height {:width 10 :height 10} {:width 50 :height 50})))))

(t/deftest test-fit-to-width
  (t/testing "Small rect within larger rect, integer scale"
    (t/is (= {:width 100 :height 20.0 :x 0 :y 40.0 :scale 2.0}
             (sut/fit-to-width {:width 100 :height 100} {:width 50 :height 10}))))

  (t/testing "Small rect within larger rect, fraction scale"
    (t/is (= {:width 100 :height 200.0004 :x 0 :y -50.0002 :scale 16.6667}
             (sut/fit-to-width {:width 100 :height 100} {:width 6 :height 12}))))

  (t/testing "Large rect within smaller rect"
    (t/is (= {:width 10 :height 10.0 :x 0 :y 0.0 :scale 0.2}
             (sut/fit-to-width {:width 10 :height 10} {:width 50 :height 50}))))

  (t/testing "Large rect within smaller rect of different aspect ratio"
    (t/is (= {:width 10 :height 10.0 :x 0 :y 5.0 :scale 0.2}
             (sut/fit-to-width {:width 10 :height 20} {:width 50 :height 50})))))

(t/deftest test-fit-to-dimensions
  (t/testing "Small rect within larger rect, integer scale"
    (t/is (= {:width 100 :height 20.0 :x 0 :y 40.0 :scale 2.0}
             (sut/fit-to-dimensions {:width 100 :height 100} {:width 50 :height 10}))))

  (t/testing "Small rect within larger rect, fraction scale"
    (t/is (= {:width 20.0 :height 100 :x 40.0 :y 0 :scale 2.0}
             (sut/fit-to-dimensions {:width 100 :height 100} {:width 10 :height 50}))))

  (t/testing "Large rect within smaller rect"
    (t/is (= {:width 10.0 :height 10 :x 0.0 :y 0 :scale 0.2}
             (sut/fit-to-dimensions {:width 10 :height 10} {:width 50 :height 50}))))

  (t/testing "Exmaple from size card- smaller then region but same aspect ratio"
    (t/is (= {:width 58.0 :height 58 :x 0.0 :y 0 :scale 29.0}
             (sut/fit-to-dimensions {:width 58 :height 58} {:width 2 :height 2})))))

(t/deftest test-position-to-dimensions
  (t/testing "Position within long rectangle"
    (t/is (= {:width 173, :height 58, :x 72.0, :y 0, :scale 29.0}
             (sut/position-to-dimensions {:width 173 :height 58} {:width 1 :height 2})))))


(t/deftest test-reverse-transform
  (t/testing "Test simple scale and translate, taken from logs"
    (t/is (= {:x 0.6424377491106995, :y 1.714310204431492}
             (sut/reverse-transform 177 4 {:x 175.501 :y 0 :scale 2.3333})))
    (t/is (= {:x 8.356833669052413, :y 1.714310204431492}
             (sut/reverse-transform 195 4 {:x 175.501 :y 0 :scale 2.3333})))))


;; ======================== Shape functions =================================


(t/deftest test-translate-shapes
  (t/testing "Simple x y translation check"
    (t/is (= [{:shape :point :x 14 :y 7}
              {:shape :point :x 24 :y 2}
              {:shape :point :x 34 :y -3}]
           (sut/translate-shapes [{:shape :point :x 10 :y 10}
                                  {:shape :point :x 20 :y 5}
                                  {:shape :point :x 30 :y 0}]
                                 4 -3)))))
