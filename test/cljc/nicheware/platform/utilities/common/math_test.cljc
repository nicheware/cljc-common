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
             (sut/round 2))))

  #?(:clj
     (t/testing "JVM: round a Ratio exactly, ties toward +infinity like Math/round"
       (t/is (= 3 (sut/round 5/2)))
       (t/is (= -2 (sut/round -5/2)))
       (t/is (= 1 (sut/round 2/3)))
       (t/is (= Long (type (sut/round 5/2))) "Whole result is a Long, not a BigInt")
       (doseq [n (range -200 201)
               :let [value (/ n 100)]]
         (t/is (= (Math/round (float value)) (sut/round value)) (str "Agrees with the float path for " value))))))

(t/deftest test-ceil
  (t/testing "Ensure returned ceiling"
    (t/is (= 2
             (sut/ceil 1.2))))

  #?(:clj
     (t/testing "JVM: ceiling of exact values"
       (t/is (= 2 (sut/ceil 3/2)))
       (t/is (= -1 (sut/ceil -3/2)))
       (t/is (= 3 (sut/ceil 3)))
       (t/is (= Long (type (sut/ceil 3/2)))))))

(t/deftest test-floor
  (t/testing "Ensure returned floor"
    (t/is (= 1
             (sut/floor 1.8))))

  #?(:clj
     (t/testing "JVM: floor of exact values"
       (t/is (= 1 (sut/floor 3/2)))
       (t/is (= -2 (sut/floor -3/2)))
       (t/is (= -1 (sut/floor -1/2)))
       (t/is (= 3 (sut/floor 3)))
       (t/is (= Long (type (sut/floor -3/2)))))))


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
             (sut/diff 2 1))))

  (t/testing "Floating point difference"
    (t/is (= 1.5 (sut/diff 0.5 2.0))))

  #?(:clj
     (t/testing "JVM: Ratio difference stays exact (Math/abs would throw)"
       (t/is (= 3/2 (sut/diff 1/2 2)))
       (t/is (= 1/2 (sut/diff 0 -1/2))))))

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

;; ======================== Exact fractions =====================================
;;
;; Portable assertions write expected values as (/ n d): an exact Ratio on the JVM and the
;; equivalent double in ClojureScript. JVM-only exactness (type) checks are under #?(:clj ...).

(defn- no-double?
  "JVM: true if value is an exact Long, BigInt or Ratio (never a Double)."
  [value]
  #?(:clj (or (instance? Long value) (ratio? value) (instance? clojure.lang.BigInt value))
     :cljs true))

(defn- illegal-arg-thrown?
  [f]
  (try (f) false
       (catch #?(:clj IllegalArgumentException :cljs js/Error) _ true)))

(t/deftest test-round-to-increment
  (t/testing "Round up to increment"
    (t/is (= (/ 3 8) (sut/round-to-increment (/ 13 36) (/ 1 8) :up)))
    (t/is (= (/ 3 8) (sut/round-to-increment (/ 3 8) (/ 1 8) :up)) "Exact multiple unchanged"))

  (t/testing "Round down to increment"
    (t/is (= 2 (sut/round-to-increment (/ 5 2) 1 :down)))
    (t/is (= (/ 1 4) (sut/round-to-increment (/ 13 36) (/ 1 8) :down))))

  (t/testing "Round to nearest increment, ties round up"
    (t/is (= (/ 1 8) (sut/round-to-increment (/ 1 16) (/ 1 8) :nearest)))
    (t/is (= (/ 3 8) (sut/round-to-increment (/ 13 36) (/ 1 8) :nearest)))
    (t/is (= 3 (sut/round-to-increment (/ 5 2) 1 :nearest)))
    (t/is (= -2 (sut/round-to-increment (/ -5 2) 1 :nearest)) "Negative tie rounds toward +infinity"))

  (t/testing "Negative values"
    (t/is (= (/ -1 4) (sut/round-to-increment (/ -13 36) (/ 1 8) :up)))
    (t/is (= (/ -3 8) (sut/round-to-increment (/ -13 36) (/ 1 8) :down)))
    (t/is (= -3 (sut/round-to-increment (/ -5 2) 1 :down))))

  (t/testing "Zero"
    (t/is (zero? (sut/round-to-increment 0 (/ 1 8) :up))))

  (t/testing "Double inputs use Math rounding"
    (t/is (= 0.25 (sut/round-to-increment 0.3 0.25 :nearest)))
    (t/is (= 0.5 (sut/round-to-increment 0.3 0.25 :up))))

  (t/testing "Invalid arguments"
    (t/is (illegal-arg-thrown? #(sut/round-to-increment 1 0 :up)))
    (t/is (illegal-arg-thrown? #(sut/round-to-increment 1 -1 :up)))
    (t/is (illegal-arg-thrown? #(sut/round-to-increment 1 (/ 1 8) :sideways)))
    (t/is (illegal-arg-thrown? #(sut/round-to-increment nil (/ 1 8) :up))))

  #?(:clj
     (t/testing "JVM exact inputs give exact results, whole results as Long"
       (t/is (ratio? (sut/round-to-increment 13/36 1/8 :up)))
       (t/is (instance? Long (sut/round-to-increment 5/2 1 :down)))
       (t/is (instance? Long (sut/round-to-increment 15/16 1/8 :nearest)))
       (t/is (instance? Long (sut/round-to-increment 0 1/8 :up)))
       (t/is (= 3/8 (sut/round-to-increment 13/36 1/8 :up)))
       (t/is (every? no-double?
                     (for [n (range -40 41)
                           direction [:up :down :nearest]]
                       (sut/round-to-increment (/ n 36) 1/8 direction))))
       (t/is (= 1000000000000000000001/8
                (sut/round-to-increment 100000000000000000000003/800 1/8 :up))
             "BigInt numerators stay exact")
       (t/is (instance? Double (sut/round-to-increment 13/36 0.125 :up))
             "A double increment gives a double result"))))

(t/deftest test-round-to-increment-exact-properties
  #?(:clj
     (t/testing "JVM: result is a multiple of increment within one increment of value, in the right direction"
       (doseq [n (range -100 101)
               :let [value     (/ n 37)
                     increment 1/8]]
         (let [up      (sut/round-to-increment value increment :up)
               down    (sut/round-to-increment value increment :down)
               nearest (sut/round-to-increment value increment :nearest)]
           (t/is (integer? (/ up increment)))
           (t/is (integer? (/ down increment)))
           (t/is (<= down value up))
           (t/is (< (- up down) (* 2 increment)))
           (t/is (<= (Math/abs (double (- nearest value))) (/ increment 2)))))))
  (t/testing "Portable: :nearest agrees with :up or :down"
    (doseq [n (range -20 21)
            :let [value (/ n 7)]]
      (t/is (contains? (hash-set (sut/round-to-increment value (/ 1 4) :up)
                                 (sut/round-to-increment value (/ 1 4) :down))
                       (sut/round-to-increment value (/ 1 4) :nearest))))))

(t/deftest test-to-ratio
  (t/testing "Doubles converted to nearest simple fraction"
    (t/is (= (/ 1 4) (sut/to-ratio 0.25)))
    (t/is (= (/ 1 3) (sut/to-ratio 0.3333333333333333)))
    (t/is (= (/ 5 2) (sut/to-ratio 2.5)))
    (t/is (= (/ -3 8) (sut/to-ratio -0.375)))
    (t/is (= 3 (sut/to-ratio 3.0)))
    (t/is (= 0 (sut/to-ratio 0.001))))

  (t/testing "Max denominator respected"
    (t/is (= (/ 1 8) (sut/to-ratio 0.1 8)))
    (t/is (= (/ 1 10) (sut/to-ratio 0.1)))
    (t/is (= 0 (sut/to-ratio 0.2 2)) "Equal error to 0 and 1/2: smallest denominator wins")
    (t/is (= 1 (sut/to-ratio 0.9 1))))

  (t/testing "Integers returned unchanged"
    (t/is (= 3 (sut/to-ratio 3))))

  (t/testing "Invalid arguments"
    (t/is (illegal-arg-thrown? #(sut/to-ratio 0.5 0)))
    (t/is (illegal-arg-thrown? #(sut/to-ratio nil)))
    (t/is (illegal-arg-thrown? #(sut/to-ratio (/ 0.0 0.0)))))

  #?(:clj
     (t/testing "JVM returns exact values, exact inputs unchanged"
       (t/is (ratio? (sut/to-ratio 0.25)))
       (t/is (instance? Long (sut/to-ratio 3.0)))
       (t/is (instance? Long (sut/to-ratio 3)))
       (t/is (= 1/127 (sut/to-ratio 1/127)) "Exact ratio unchanged regardless of denominator")
       (t/is (= 3/2 (sut/to-ratio 1.5M)) "BigDecimal is treated as a non-exact value"))))

(t/deftest test-format-fraction
  (t/testing "Ascii (default) style"
    (t/is (= "2 1/2" (sut/format-fraction (/ 5 2))))
    (t/is (= "1/4" (sut/format-fraction (/ 1 4))))
    (t/is (= "44" (sut/format-fraction 44)))
    (t/is (= "0" (sut/format-fraction 0)))
    (t/is (= "-2 1/2" (sut/format-fraction (/ -5 2))))
    (t/is (= "-1/4" (sut/format-fraction (/ -1 4))))
    (t/is (= "1 3/8" (sut/format-fraction (/ 11 8))))
    (t/is (= "1 3/8" (sut/format-fraction (/ 11 8) {:style :ascii}))))

  (t/testing "Unicode style"
    (t/is (= "2½" (sut/format-fraction (/ 5 2) {:style :unicode})))
    (t/is (= "¼" (sut/format-fraction (/ 1 4) {:style :unicode})))
    (t/is (= "1⅜" (sut/format-fraction (/ 11 8) {:style :unicode})))
    (t/is (= "-2½" (sut/format-fraction (/ -5 2) {:style :unicode})))
    (t/is (= "44" (sut/format-fraction 44 {:style :unicode})))
    (t/is (= "1 5/16" (sut/format-fraction (/ 21 16) {:style :unicode})) "No glyph falls back to ascii"))

  (t/testing "Doubles converted with to-ratio"
    (t/is (= "3/8" (sut/format-fraction 0.375)))
    (t/is (= "2 1/3" (sut/format-fraction 2.3333333333)))
    (t/is (= "1/8" (sut/format-fraction 0.1 {:max-denominator 8})))
    (t/is (= "⅛" (sut/format-fraction 0.1 {:style :unicode :max-denominator 8}))))

  (t/testing "Invalid arguments"
    (t/is (illegal-arg-thrown? #(sut/format-fraction 1 {:style :latex})))
    (t/is (illegal-arg-thrown? #(sut/format-fraction nil))))

  #?(:clj
     (t/testing "JVM exact ratio formatted regardless of max-denominator"
       (t/is (= "1/127" (sut/format-fraction 1/127 {:max-denominator 8}))))))

(t/deftest test-parse-mixed-fraction
  (t/testing "Accepted forms"
    (t/is (= 44 (sut/parse-mixed-fraction "44")))
    (t/is (= (/ 1 4) (sut/parse-mixed-fraction "1/4")))
    (t/is (= (/ 5 2) (sut/parse-mixed-fraction "5/2")))
    (t/is (= (/ 3 2) (sut/parse-mixed-fraction "6/4")))
    (t/is (= (/ 5 2) (sut/parse-mixed-fraction "2 1/2")))
    (t/is (= (/ 5 2) (sut/parse-mixed-fraction "2½")))
    (t/is (= (/ 5 2) (sut/parse-mixed-fraction "2 ½")))
    (t/is (= (/ 1 2) (sut/parse-mixed-fraction "½")))
    (t/is (= (/ 11 8) (sut/parse-mixed-fraction "1⅜")))
    (t/is (= (/ 1 10) (sut/parse-mixed-fraction "⅒")))
    (t/is (= 0 (sut/parse-mixed-fraction "0"))))

  (t/testing "Negative forms"
    (t/is (= -44 (sut/parse-mixed-fraction "-44")))
    (t/is (= (/ -1 4) (sut/parse-mixed-fraction "-1/4")))
    (t/is (= (/ -5 4) (sut/parse-mixed-fraction "-1 1/4")))
    (t/is (= (/ -5 2) (sut/parse-mixed-fraction "-2½")))
    (t/is (= (/ -1 2) (sut/parse-mixed-fraction "-½"))))

  (t/testing "Surrounding whitespace trimmed"
    (t/is (= (/ 5 2) (sut/parse-mixed-fraction "  2 1/2 \t"))))

  (t/testing "Rejected forms return nil"
    (doseq [s ["0.25" "1/0" "2 1/0" "" "   " "2 1/2 3" "44\"" "2  1/2" "2 3/2" "2 0/4"
               "+2" "- 2" "1/2/3" "abc" "2 ½ ½" "2½\"" "1 /2"]]
      (t/is (nil? (sut/parse-mixed-fraction s)) (str "Should reject: " (pr-str s))))
    (t/is (nil? (sut/parse-mixed-fraction nil)))
    (t/is (nil? (sut/parse-mixed-fraction 44))))

  #?(:clj
     (t/testing "JVM returns exact values"
       (t/is (instance? Long (sut/parse-mixed-fraction "44")))
       (t/is (instance? Long (sut/parse-mixed-fraction "8/4")))
       (t/is (ratio? (sut/parse-mixed-fraction "2 1/2")))
       (t/is (ratio? (sut/parse-mixed-fraction "2½")))
       (t/is (= 99999999999999999999999N (sut/parse-mixed-fraction "99999999999999999999999"))))))

(t/deftest test-format-parse-round-trip
  (t/testing "parse-mixed-fraction inverts format-fraction for both styles"
    (doseq [style [:ascii :unicode]
            d     [1 2 3 4 5 6 7 8 9 10 12 16 32 64]
            n     (range -70 71)
            :let  [r (/ n d)]]
      (t/is (= r (sut/parse-mixed-fraction (sut/format-fraction r {:style style})))
            (str "Round trip " style " " n "/" d)))))
