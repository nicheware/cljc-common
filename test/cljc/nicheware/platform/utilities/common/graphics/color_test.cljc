(ns nicheware.platform.utilities.common.graphics.color-test
  (:require [nicheware.platform.utilities.common.graphics.color :as sut]
            [nicheware.platform.utilities.common.math :as math]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(t/deftest test-normalise-rgb
  (t/testing "Normalise lowest, middle and top"
    (t/is (= 1.0
             (sut/normalise-rgb 255)))
    (t/is (= 0.0
             (sut/normalise-rgb 0)))
    (t/is (= 0.5
             (math/roundn 2 (sut/normalise-rgb 127))))))

;; Remove when remove from color
(comment
  (t/deftest test-value-as-rgba
    (t/testing "Test a vector of RGB Int"
      (t/is (= [1.0 0.004 0.0 1.0]
               (map #(math/roundn 3 %)
                    @(sut/value-as-rgba [255 1 0])))))

    (t/testing "Test a packed Int <A><R><G><B>"
      (t/is (= [1.0 0.004 0.0 1.0]
               (map #(math/roundn 3 %)
                    @(sut/value-as-rgba 0xFFFF0100)))))))


(t/deftest test-color-as-rgba
  (t/testing "Test a vector of RGB Int"
    (t/is (= [255 1 0 255]
             (sut/color-as-rgba [255 1 0]))))

  (t/testing "Test a packed Int <A><R><G><B>"
    (t/is (= [255 1 0 255]
             (sut/color-as-rgba 0xFFFF0100)))))

(t/deftest test-make-color-increment-fn
  (t/testing "Create a function halfway"
    (let [inc-fn (sut/make-color-increment-fn [0 0 0 0] [127 127 127 127])]
      (t/is (= [127 127 127 127]
               (inc-fn [0 0 0 0])))

      (t/is (= [254 254 254 254]
               (last (take 2 (iterate inc-fn [127 127 127 127])))))

      (t/is (= [255 255 255 255]
               (last (take 3 (iterate inc-fn [127 127 127 127]))))))))

(t/deftest test-color-as-map
  (t/testing "Test a vector of RGB Int"
    (t/is (= {:r 255 :g 1 :b 0 :a 1.0}
             (sut/color-as-map [255 1 0]))))

  (t/testing "Test a packed Int <A><R><G><B>"
    (t/is (= {:r 255 :g 1 :b 0 :a 1.0}
             (sut/color-as-map 0xFFFF0100)))))

(t/deftest test-unique-color
  (t/testing "Test given three existing colors"
    (t/is (= [223 224 223 255]
             (sut/unique-color [223 224 225 255]
                               [[223 224 224 255] [223 224 225 255] [223 224 223 254]])))))

;; ================================= Color model conversions ==============

(t/deftest test-rgba-to-hsla
  (t/testing "Convert basic colors - red, magenta, teal"
    (t/is (= [0 1.0 0.5 255]
             (sut/rgba-to-hsla [255 0 0 255])))
    (t/is (= [300 1.0 0.5 255]
             (sut/rgba-to-hsla [255 0 255 255])))
    (t/is (= [180 1.0 0.25 255]
             (sut/rgba-to-hsla [0 128 128 255]))))

  ;; See calculator at http://www.rapidtables.com/convert/color/rgb-to-hsl.htm
  (t/testing "Random color combinations found in calculator"
    (t/testing " dark green"
      (t/is (= [139 0.69 0.3 255]
               (sut/rgba-to-hsla [23 129 57 255]))))

    (t/testing " navy blue"
      (t/is (= [235 0.59 0.56 255]
               (sut/rgba-to-hsla [78 89 210 255]))))

    (t/testing " dark blue"
      (t/is (= [235 0.59 0.56 255]
               (sut/rgba-to-hsla [78 89 210 255]))))))

(t/deftest test-hsls-to-rgba
  (t/testing "Convert basic colors - red, magenta, teal"
    (t/is (= [255 0 0 255]
             (sut/hsla-to-rgba [0 1.0 0.5 255])))
    (t/is (= [255 0 255 255]
             (sut/hsla-to-rgba [300 1.0 0.5 255])))
    (t/is (= [0 128 128 255]
             (sut/hsla-to-rgba [180 1.0 0.25 255]))))

  (t/testing "Random color combinations found in calculator"
    (t/testing " dark green"
      (t/is (= [24 129 57 255]
               (sut/hsla-to-rgba [139 0.69 0.3 255]))))

    (t/testing " navy blue"
      (t/is (= [77 88 209 255]
               (sut/hsla-to-rgba [235 0.59 0.56 255]))))

    (t/testing " dark blue"
      (t/is (= [77 88 209 255]
               (sut/hsla-to-rgba [235 0.59 0.56 255]))))))
