(ns nicheware.platform.utilities.common.math
  "
  Functions complementing those in the Clojure/Clojurescript Math namespace.

There are groups of functions and variables within math that deal with:

|Function group|Functions|
|---|---|
|value manipulations| [[roundn]], [[round]], [[ceil]], [[floor]], [[div]], [[diff]], [[mult]], [[clamp-change]], [[max-abs]]|
|sequence generators| [[make-ratio-sequence-fn]], [[ratio-sequence]]|
|exact fractions| [[round-to-increment]], [[to-ratio]], [[format-fraction]], [[parse-mixed-fraction]]|

  Where suitable, uses Math/ functions.

## Exact fractions

On the JVM the exact-fraction functions keep exact values (Long, BigInt, Ratio) exact:
no computation passes through a double, so they can be used where every intermediate value
must be an exact rational (eg measurements in 1/8 inch increments).
ClojureScript has no ratio type, so there the same functions work on (and return) doubles.
[[round]], [[ceil]], [[floor]] and [[diff]] follow the same rule: given an exact value on the JVM they
never pass through a double.

```clojure
(round-to-increment 13/36 1/8 :up)        => 3/8
(to-ratio 0.25)                           => 1/4
(format-fraction 11/8)                    => \"1 3/8\"
(format-fraction 11/8 {:style :unicode})  => \"1⅜\"
(parse-mixed-fraction \"2 1/2\")            => 5/2
(parse-mixed-fraction \"2½\")               => 5/2
```
"
  (:require [clojure.string :as str]
            [nicheware.platform.utilities.common.core :as core]))

;; ======================== Value manipulations  ================================


(defn roundn
  "Rounds a float to the given precision.

  - precision: Number of decimal places to round to. 0 will result in int.
  - num: float to be rounded.
  - returns: num rounded to precision.

eg:
```clojure
(roundn 1.54364 3) => 1.544
```
"
  [precision num]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* num factor)) factor)))

(declare exact? normalise-exact #?@(:clj [exact-floor exact-ceil]))

(defn round
  "Rounds a value, handling floats and ints and always returning an int.

  - value: Value to be rounded. Can be float, int or (Clojure only) Ratio.
  - returns: rounded value as an int. Ties (half way) round toward +infinity, eg -5/2 => -2.
    On the JVM an exact value (Long, BigInt, Ratio) is rounded with integer arithmetic only, no double is used."
  [value]
  (if (exact? value)
    #?(:clj (normalise-exact (exact-floor (+ value 1/2)))
       :cljs nil)
    (Math/round (float value))))

(defn ceil
  "Ceiling call which converts return value to int to simplify later type checking.

  - value: Value as source for ceiling.
  - returns: ceiling of value as an int.
    On the JVM an exact value (Long, BigInt, Ratio) uses integer arithmetic only, no double is used.

eg:
```clojure
  (ceil 1.3) => 2
```
"
  [value]
  (if (exact? value)
    #?(:clj (normalise-exact (exact-ceil value))
       :cljs nil)
    (int (Math/ceil value))))

(defn floor
  "Floor call which converts return value to int to simplify later type checking.

  - value: Value as source for floor.
  - returns: floor of value as an int.
    On the JVM an exact value (Long, BigInt, Ratio) uses integer arithmetic only, no double is used.

eg:
```clojure
  (floor 1.7) => 1
```
"
  [value]
  (if (exact? value)
    #?(:clj (normalise-exact (exact-floor value))
       :cljs nil)
    (int (Math/floor value))))

(defn div
  "Do division in format consistent between ClojureScript (which has no ratio) and Clojure.

  - numer: Numerator for division.
  - denom: Denominator for division.
  - returns: float of result of division. (java.lang.Double for JVM)

eg:
```clojure
  (div 1 2) => 0.5
```
"
  [numer denom]
  (roundn 4 (/ numer denom)))

(defn diff
  "Absolute difference.

  - values1: First value to be used in difference.
  - value2: Second value to be used in difference.
  - returns: abs(value2 - value1), of the same numeric type, so an exact (Long, BigInt, Ratio) difference stays exact.

eg:
```clojure
  (diff 1 2) => 1
  (diff 1/2 2) => 3/2
```
"
  [value1 value2]
  ;; Not Math/abs, which has no Ratio overload and throws.
  (let [difference (- value1 value2)]
    (if (neg? difference) (- difference) difference)))

(defn mult
  "Multiply to 4 decimal places.

  - arg1: first arg to be multiplied.
  - arg2: second arg to be multiplied.
  - returns: arg1 * arg2 with precision of 4.

eg:
```clojure
  (mult 1.74 2.887) => 5.0234
```
"
  [arg1 arg2]
  (roundn 4 (* arg1 arg2)))

(defn clamp-change
  "Clamps the change (positive or negative) to the specified absolute magnitude.

  - start: start of difference calculation.
  - end: end of difference calculation.
  - max-abs-change: Maximum absolute difference to be returned.
  - returns: start + max of (end - start) and max-abs-change

eg:
```clojure
  (clamp-change 1 9 6) => 7
  (clamp-change 9 1 6) => 3
  (clamp-change 1 9 10) => 9
```
"
  [start end max-abs-change]
  (let [change (- end start)]
    (if (< change 0)
      (+ start (max change (- 0 max-abs-change)))
      (+ start (min change max-abs-change)))))

(defn max-abs
  "Return whichever value is the biggest absolute value, retaining the sign on the returned value.

   - val1: First input value to compare against.
   - val2: Second input value to compare against.
   - returns: either val1 or val2, whichever has biggest absolute value.

eg:
```clojure
max-abs(-1 -2) => -2
max-abs(1 2)   => 2
max-abs(1 -2)  => -2
```
"
  [val1 val2]
  (if (> (Math/abs val1) (Math/abs val2))
    val1
    val2))


;; ======================== Sequence generators =================================

(defn make-ratio-sequence-fn
  "Creates a function which accepts arguments 0, 1, ....n
   generating a sequence where each value is a progressive ratio of the supplied total.

   eg ratio = 0.5, total 10, then

   - initial point is half of total (0.5)
   - next point is half of remaining distance (7.5)
   - next is half again of remaining (8.75) etc

  f(x) = f(x-1) + ratio * (total - f(x-1))

  - ratio: float value used to compute next point between current point and total.
  - total: end value used to computer intermediate point. No value will be greater than this.
  - returns: a function that accepts a single int argument, indicating which nth value is required.
    Will compute ratio n times, starting between 0 and total, and then taking result as new starting point.
"
  [ratio total]
  (let [initial-value (* total ratio)])
  (fn [n]
    (loop [previous-value 0
           current-n 0
           target-n n]
      (let [value (case current-n
                    0 0
                    1 (* total ratio)
                    (+ previous-value (* ratio (- total previous-value))))]
        (if (= current-n target-n)
          value
          (recur value (inc current-n) target-n))))))


(defn ratio-sequence
  "Makes a lazy infinite sequence from the ratio function, producing values
   starting at start and getting closer to end (but never reaching).

  - ratio: float value used to compute next point between current point and end.
  - start: initial value in sequence
  - end: End value, which will never be reached.
  - returns: lazy sequence of points between start and end, approaching end using ratio.
    see [[make-ratio-sequence-fn]] for details on function used to produce points.

eg:
```clojure
(take 10 (ratio-sequence 0.5 0 20))
 =>
(0 10.0 15.0 17.5 18.75 19.375 19.6875 19.84375 19.921875 19.9609375)
```

"
  [ratio start end]
  (let [sequence-fn (make-ratio-sequence-fn ratio (- end start))]
    (map #(+ start (sequence-fn %)) (range))))


;; ======================== Exact fractions =====================================

(defn ^:no-doc exact?
  "True if value is an exact number (integer or Ratio). Always false in ClojureScript,
  which has no exact non-integer type."
  [value]
  #?(:clj (or (integer? value) (ratio? value))
     :cljs false))

(defn ^:no-doc finite-number?
  "True if value is a number that is neither NaN nor infinite."
  [value]
  (and (number? value)
       #?(:clj (or (exact? value)
                   (let [d (double value)]
                     (not (or (Double/isNaN d) (Double/isInfinite d)))))
          :cljs (js/isFinite value))))

(defn ^:no-doc normalise-exact
  "Clojure ratio arithmetic returns whole results as BigInt (eg (* 1/8 8) => 1N).
  Converts a BigInt that fits in a long to a Long, leaving all other values unchanged."
  [value]
  #?(:clj (if (and (instance? clojure.lang.BigInt value)
                   (nil? (.bipart ^clojure.lang.BigInt value)))
            (long value)
            value)
     :cljs value))

#?(:clj
   (defn ^:no-doc exact-floor
     "Floor of an exact rational computed with integer arithmetic only (no doubles).

     - value: Long, BigInt or Ratio.
     - returns: the largest integer <= value."
     [value]
     (if (ratio? value)
       (let [n (numerator value)
             q (quot n (denominator value))]
         ;; quot truncates toward zero, a non-integral negative ratio must step down one.
         (if (neg? n) (dec' q) q))
       value)))

#?(:clj
   (defn ^:no-doc exact-ceil
     "Ceiling of an exact rational computed with integer arithmetic only (no doubles).

     - value: Long, BigInt or Ratio.
     - returns: the smallest integer >= value."
     [value]
     (if (ratio? value)
       (let [n (numerator value)
             q (quot n (denominator value))]
         (if (pos? n) (inc' q) q))
       value)))

#?(:clj
   (defn ^:no-doc exact-increment-count
     "Number of whole increments in the exact quotient value/increment, for the rounding direction,
     using integer arithmetic only."
     [quotient direction]
     (case direction
       :up      (exact-ceil quotient)
       :down    (exact-floor quotient)
       :nearest (exact-floor (+ quotient (/ 1 2))))))

(defn ^:no-doc double-increment-count
  "Number of whole increments in the double quotient value/increment, for the rounding direction."
  [quotient direction]
  (case direction
    :up      (Math/ceil quotient)
    :down    (Math/floor quotient)
    :nearest (Math/floor (+ quotient 0.5))))

(def ^:no-doc round-directions #{:up :down :nearest})

(defn round-to-increment
  "Rounds a value to a multiple of the given increment.

  - value: number to be rounded.
  - increment: positive number, the unit to which value is rounded (eg 1/8).
  - direction: one of
    - :up - smallest multiple of increment >= value
    - :down - largest multiple of increment <= value
    - :nearest - nearest multiple of increment, with exact ties (half way) rounding up (toward +infinity).
  - returns: the multiple of increment.
    On the JVM when both value and increment are exact (Long, BigInt, Ratio) the computation uses only
    integer arithmetic and the result is exact: a Long/BigInt when whole, otherwise a Ratio.
    If either is a double (always the case in ClojureScript) Math/floor and Math/ceil are used and a
    double is returned.
  - throws: IllegalArgumentException (js/Error in ClojureScript, see [[nicheware.platform.utilities.common.core/throw-illegal-arg]])
    if value is not a finite number, increment is not a positive finite number, or direction is unknown.

eg:
```clojure
(round-to-increment 13/36 1/8 :up)     => 3/8
(round-to-increment 3/8 1/8 :up)       => 3/8
(round-to-increment 1/16 1/8 :nearest) => 1/8
(round-to-increment 5/2 1 :down)       => 2
(round-to-increment 0.3 0.25 :nearest) => 0.25
```
"
  [value increment direction]
  (when-not (finite-number? value)
    (core/throw-illegal-arg "round-to-increment value must be a finite number"))
  (when-not (and (finite-number? increment) (pos? increment))
    (core/throw-illegal-arg "round-to-increment increment must be a positive number"))
  (when-not (contains? round-directions direction)
    (core/throw-illegal-arg (str "round-to-increment direction must be one of :up :down :nearest, not: "
                                 (pr-str direction))))
  (if (and (exact? value) (exact? increment))
    #?(:clj (normalise-exact (* increment (exact-increment-count (/ value increment) direction)))
       :cljs nil)
    (let [increment (double increment)]
      (* increment (double-increment-count (/ (double value) increment) direction)))))

(def ^:no-doc default-max-denominator 64)

(defn ^:no-doc best-approximation
  "Finds the fraction n/d, 1 <= d <= max-denominator, nearest to the double x.
  On equal error the smallest d wins, which also guarantees n/d is in lowest terms.

  - x: finite number (treated as a double).
  - max-denominator: positive integer.
  - returns: [n d] as integers (Long on the JVM, integral doubles in ClojureScript)."
  [x max-denominator]
  (let [x (double x)]
    (->> (range 1 (inc max-denominator))
         (map (fn [d]
                (let [n (Math/round (* x d))]
                  {:n n :d d :error (Math/abs (- x (/ (double n) d)))})))
         (reduce (fn [best candidate]
                   (if (< (:error candidate) (:error best)) candidate best)))
         ((juxt :n :d)))))

(defn ^:no-doc validate-max-denominator
  [max-denominator]
  (when-not (and (integer? max-denominator) (pos? max-denominator))
    (core/throw-illegal-arg "max-denominator must be a positive integer")))

(defn to-ratio
  "Converts a number to the nearest simple fraction.

  - x: number to convert.
  - max-denominator: optional positive integer, the largest denominator permitted. Default 64.
  - returns: the rational n/d, 1 <= d <= max-denominator, nearest to x. When two fractions are equally
    near, the one with the smallest denominator is returned.
    Exact inputs (integer or Ratio on the JVM) are returned unchanged, regardless of their denominator.
    On the JVM the result is a Long or Ratio; in ClojureScript it is the equivalent double.
  - throws: IllegalArgumentException (js/Error in ClojureScript) if x is not a finite number or
    max-denominator is not a positive integer.

eg:
```clojure
(to-ratio 0.25)               => 1/4
(to-ratio 0.3333333333333333) => 1/3
(to-ratio 2.5)                => 5/2
(to-ratio 3)                  => 3
(to-ratio 0.1 8)              => 1/8
```
"
  ([x] (to-ratio x default-max-denominator))
  ([x max-denominator]
   (validate-max-denominator max-denominator)
   (when-not (finite-number? x)
     (core/throw-illegal-arg "to-ratio value must be a finite number"))
   (if (exact? x)
     x
     (let [[n d] (best-approximation x max-denominator)]
       (/ n d)))))

;; Unicode vulgar fraction glyphs, keyed by [numerator denominator] in lowest terms.
(def ^:no-doc fraction-glyphs
  {[1 2] "½", [1 3] "⅓", [2 3] "⅔", [1 4] "¼", [3 4] "¾",
   [1 5] "⅕", [2 5] "⅖", [3 5] "⅗", [4 5] "⅘", [1 6] "⅙", [5 6] "⅚",
   [1 7] "⅐", [1 8] "⅛", [3 8] "⅜", [5 8] "⅝", [7 8] "⅞", [1 9] "⅑", [1 10] "⅒"})

(def ^:no-doc glyph-fractions
  (into {} (map (fn [[fraction glyph]] [glyph fraction])) fraction-glyphs))

(defn ^:no-doc fraction-parts
  "Numerator and denominator (in lowest terms, denominator positive) of a number.
  Exact values are used as is; other numbers are first approximated as in [[to-ratio]]."
  [value max-denominator]
  #?(:clj (cond
            (ratio? value)   [(numerator value) (denominator value)]
            (integer? value) [value 1]
            :else            (best-approximation value max-denominator))
     :cljs (best-approximation value max-denominator)))

(defn ^:no-doc format-proper-fraction
  "Formats a proper fraction n/d (0 < n < d) following a whole part, in the given style.
  Unicode uses a glyph (no separating space) when one exists, otherwise falls back to ascii."
  [whole n d style]
  (let [glyph      (when (= style :unicode) (get fraction-glyphs [n d]))
        whole-part (when-not (zero? whole) (str whole))]
    (if glyph
      (str whole-part glyph)
      (str whole-part (when whole-part " ") n "/" d))))

(def ^:no-doc fraction-styles #{:ascii :unicode})

(defn format-fraction
  "Formats a number as a mixed-number string, eg \"2 1/2\". No units are added.

  - value: number to format.
  - opts: optional map of
    - :style - :ascii (default) formats the fraction as n/d separated from the whole part by a space.
      :unicode uses a single vulgar-fraction glyph (½ ⅓ ⅔ ¼ ¾ ⅕ ⅖ ⅗ ⅘ ⅙ ⅚ ⅐ ⅛ ⅜ ⅝ ⅞ ⅑ ⅒) with no
      space after the whole part, falling back to the ascii form when the fraction has no glyph.
    - :max-denominator - positive integer, default 64. A non-exact value (a double, which is always the
      case in ClojureScript) is first converted with [[to-ratio]] using this as the max denominator.
  - returns: string of the whole part (if non zero) followed by the fraction part (if non zero),
    with a leading \"-\" for negative values. Zero is \"0\".
  - throws: IllegalArgumentException (js/Error in ClojureScript) if value is not a finite number or
    an option is invalid.

eg:
```clojure
(format-fraction 5/2)                       => \"2 1/2\"
(format-fraction 1/4)                       => \"1/4\"
(format-fraction 44)                        => \"44\"
(format-fraction -5/2)                      => \"-2 1/2\"
(format-fraction 5/2 {:style :unicode})     => \"2½\"
(format-fraction 11/8 {:style :unicode})    => \"1⅜\"
(format-fraction 21/16 {:style :unicode})   => \"1 5/16\"
(format-fraction 0.375)                     => \"3/8\"
```
"
  ([value] (format-fraction value {}))
  ([value {:keys [style max-denominator]
           :or   {style :ascii max-denominator default-max-denominator}}]
   (validate-max-denominator max-denominator)
   (when-not (contains? fraction-styles style)
     (core/throw-illegal-arg (str "format-fraction style must be :ascii or :unicode, not: " (pr-str style))))
   (when-not (finite-number? value)
     (core/throw-illegal-arg "format-fraction value must be a finite number"))
   (let [[n d]     (fraction-parts value max-denominator)
         sign      (when (neg? n) "-")
         abs-n     (if (neg? n) (- n) n)
         whole     (quot abs-n d)
         remainder (rem abs-n d)]
     (if (zero? remainder)
       (str sign whole)
       (str sign (format-proper-fraction whole remainder d style))))))

(def ^:no-doc integer-pattern #"^(-)?(\d+)$")
(def ^:no-doc fraction-pattern #"^(-)?(\d+)/(\d+)$")
(def ^:no-doc mixed-pattern #"^(-)?(\d+) (\d+)/(\d+)$")
(def ^:no-doc glyph-pattern
  (re-pattern (str "^(-)?(?:(\\d+) ?)?([" (apply str (keys glyph-fractions)) "])$")))

(defn ^:no-doc parse-natural
  "Parses a string of decimal digits to an integer: Long (BigInt if too large) on the JVM, number in ClojureScript."
  [digits]
  #?(:clj (let [big (java.math.BigInteger. ^String digits)]
            (if (< (.bitLength big) 64) (.longValue big) (bigint big)))
     :cljs (js/parseInt digits 10)))

(defn ^:no-doc signed
  [minus value]
  (if minus (- value) value))

(defn ^:no-doc mixed-value
  "Value of whole + n/d, computed as a single division (whole*d + n)/d so that in ClojureScript the
  double is the same as that of (/ (+ (* whole d) n) d), ie a round trip with the improper fraction."
  [whole n d]
  (/ (+ (* whole d) n) d))

(defn ^:no-doc parse-integer-form
  "Parses \"44\" or \"-44\", nil if no match."
  [s]
  (when-let [[_ minus whole] (re-matches integer-pattern s)]
    (signed minus (parse-natural whole))))

(defn ^:no-doc parse-fraction-form
  "Parses \"n/d\" or \"-n/d\" with a non zero denominator, nil if no match."
  [s]
  (when-let [[_ minus n d] (re-matches fraction-pattern s)]
    (let [d (parse-natural d)]
      (when-not (zero? d)
        (signed minus (/ (parse-natural n) d))))))

(defn ^:no-doc parse-mixed-form
  "Parses \"w n/d\" or \"-w n/d\" where the fraction is proper (0 < n < d), nil if no match."
  [s]
  (when-let [[_ minus whole n d] (re-matches mixed-pattern s)]
    (let [n (parse-natural n)
          d (parse-natural d)]
      (when (and (pos? n) (< n d))
        (signed minus (mixed-value (parse-natural whole) n d))))))

(defn ^:no-doc parse-glyph-form
  "Parses an optional whole number, optional single space and a vulgar-fraction glyph, eg \"2½\", \"-½\".
  nil if no match."
  [s]
  (when-let [[_ minus whole glyph] (re-matches glyph-pattern s)]
    (let [[n d] (get glyph-fractions glyph)]
      (signed minus (mixed-value (if whole (parse-natural whole) 0) n d)))))

(def ^:no-doc form-parsers
  [parse-integer-form parse-fraction-form parse-mixed-form parse-glyph-form])

(defn parse-mixed-fraction
  "Parses a string holding an integer, fraction or mixed number to an exact value.

  Accepted forms (surrounding whitespace is ignored, each may have a leading \"-\"):

  - integer: \"44\"
  - fraction: \"1/4\", \"5/2\", \"6/4\" (non zero denominator)
  - mixed number, whole and proper fraction separated by a single space: \"2 1/2\"
  - whole number and vulgar-fraction glyph, with or without a single space: \"2½\", \"2 ½\"
  - lone glyph: \"½\". Glyphs are those produced by [[format-fraction]].

  Anything else (decimals, zero denominators, blank, extra terms, unit marks, non strings) is rejected.

  - s: string to parse.
  - returns: the value, as an exact Long, BigInt or Ratio on the JVM (a number in ClojureScript),
    or nil if s is not one of the accepted forms.

eg:
```clojure
(parse-mixed-fraction \"2 1/2\")  => 5/2
(parse-mixed-fraction \"2½\")     => 5/2
(parse-mixed-fraction \"44\")     => 44
(parse-mixed-fraction \"-1 1/4\") => -5/4
(parse-mixed-fraction \"6/4\")    => 3/2
(parse-mixed-fraction \"0.25\")   => nil
```
"
  [s]
  (when (string? s)
    (let [trimmed (str/trim s)]
      (some-> (some #(% trimmed) form-parsers)
              normalise-exact))))
