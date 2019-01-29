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

(t/deftest test-int-rgb
  (t/testing "Test half way value"
    (t/is (= 127
             (sut/int-rgb 0.5))))

  (t/testing "Test end values value"
    (t/is (= 255
             (sut/int-rgb 1.0)))
    (t/is (= 0
             (sut/int-rgb 0)))))


(t/deftest test-ratio-rgba-to-int-rgba
  (t/testing "Vector of ratios to int values"
    (t/is (= [10 127 100]
             (sut/ratio-rgba-to-int-rgba [20/2 127/1 300/3])))))

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

(t/deftest test-color-as-map
  (t/testing "Test a vector of RGB Int"
    (t/is (= {:r 255 :g 1 :b 0 :a 1.0}
             (sut/color-as-map [255 1 0]))))

  (t/testing "Test a packed Int <A><R><G><B>"
    (t/is (= {:r 255 :g 1 :b 0 :a 1.0}
             (sut/color-as-map 0xFFFF0100)))))

(t/deftest test-rgba-as-css-hex
  (t/testing "Test simple RGBA conversion to CSS Hex"
    (t/is (= "#ffffff"
             (sut/rgba-as-css-hex [255 255 255 0])))
    (t/is (= "#01fe10"
             (sut/rgba-as-css-hex [1 254 16 255])))))

(t/deftest test-color-as-css-hex
  (t/testing "Test RGBA to CSS Hex"
    (t/is (= "#01ff02"
             (sut/color-as-css-hex [1 255 2 255]))))

  (t/testing "Test RGB to CSS Hex"
    (t/is (= "#01ff02"
             (sut/color-as-css-hex [1 255 2]))))

  (t/testing "Test packed int ARGB"
    (t/is (= "#01ff02"
             (sut/color-as-css-hex 0xFF01FF02)))))


(t/deftest test-react-color-as-rgba
  (t/testing "Test extract colors from react color map"
    (t/is (= [1 244 2 255]
             (sut/react-color-as-rgba {"rgb" {"r" 1, "g" 244, "b" 2, "a" 1}})))))

(t/deftest test-nudge-color
  (t/testing "Test that color is different"
    (t/is (not (= [12 22 255 255]
                  (sut/nudge-color [12 22 255 255])))))

  (t/testing "Test that alpha unchanged"
    (t/is (= 255
             (nth (sut/nudge-color [12 22 255 255]) 3)))))

(t/deftest test-unique-color
  (t/testing "Test given three existing colors"
    (t/is (= [223 224 223 255]
             (sut/unique-color [223 224 225 255]
                               [[223 224 224 255] [223 224 225 255] [223 224 223 254]])))))

;; ================================= RGBA to HSLA color model conversion ===================================

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

;; ============================= Color model keyword functions ===============

(t/deftest test-model-from-to
  (t/testing "invalid conversion"
    (t/is (thrown? IllegalArgumentException
             (sut/model-from-to :rgba :unknown [78 89 210 255] ))))

  (t/testing "rgba to hsla (navy blue)"
    (t/is (= [235 0.59 0.56 255]
             (sut/model-from-to :rgba :hsla [78 89 210 255] ))))

  (t/testing "hsla to rgba (navy blue)"
    (t/is (= [77 88 209 255]
             (sut/model-from-to :hsla :rgba [235 0.59 0.56 255] ))))

  (t/testing "hsla no conversion "
    (t/is (= [235 0.59 0.56 255]
             (sut/model-from-to :hsla :hsla [235 0.59 0.56 255] ))))

  (t/testing "rgba no conversion"
    (t/is (= [78 89 210 255]
             (sut/model-from-to :rgba :rgba [78 89 210 255] )))))

(t/deftest test-model-ranges
  (t/testing "hsla ranges"
    (t/is (= [359 1 1 255]
             (sut/model-ranges :hsla))))

  (t/testing "rgba ranges"
    (t/is (= [255 255 255 255]
             (sut/model-ranges :rgba)))))

;; =========================== Color gradients =============================

(t/deftest test-make-color-increment-fn
  (t/testing "Create a function halfway"
    (let [inc-fn (sut/make-color-increment-fn [0 0 0 0] [127 127 127 127])]
      (t/is (= [127 127 127 127]
               (inc-fn [0 0 0 0])))

      (t/is (= [254 254 254 254]
               (last (take 2 (iterate inc-fn [127 127 127 127])))))

      (t/is (= [255 255 255 255]
               (last (take 3 (iterate inc-fn [127 127 127 127]))))))))

(t/deftest test-color-difference-seq
  (t/testing "Test even increment sequence"
    (t/is (= [[20 20 20 20] [30 30 30 30] [40 40 40 40]]
             (sut/color-difference-seq [0 0 0 0] [10 10 10 10] 3))))

  (t/testing "Test constant alpah"
    (t/is (= [[20 40 60 255] [30 60 90 255] [40 80 120 255]]
             (sut/color-difference-seq [0 0 0 255] [10 20 30 255] 3)))))
