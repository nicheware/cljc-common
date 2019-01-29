(ns nicheware.platform.utilities.common.math
  "
  Functions complementing those in the Clojure/Clojurescript Math namespace.

There are groups of functions and variables within math that deal with:

|Function group|Functions|
|---|---|
|value manipulations| [[roundn]], [[round]], [[ceil]], [[floor]], [[div]], [[diff]], [[mult]], [[clamp-change]], [[max-abs]]|
|sequence generators| [[make-ratio-sequence-fn]], [[ratio-sequence]]|

  Where suitable, uses Math/ functions.
"  )

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

(defn round
  "Rounds a value, handling floats and ints and always returning an int.

  - value: Value to be rounded. Can be float or int.
  - returns: rounded value as an int."
  [value]
  (Math/round (float value)))

(defn ceil
  "Ceiling call which converts return value to int to simplify later type checking.

  - value: Value as source for ceiling.
  - returns: ceiling of value as an int.

eg:
```clojure
  (ceil 1.3) => 2
```
"
  [value]
  (int (Math/ceil value)))

(defn floor
  "Floor call which converts return value to int to simplify later type checking.

  - value: Value as source for floor.
  - returns: floor of value as an int.

eg:
```clojure
  (floor 1.7) => 1
```
"
  [value]
  (int (Math/floor value)))

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
  - returns: abs(value2 - value1)

eg:
```clojure
  (diff 1 2) => 1
```
"
  [value1 value2]
  (Math/abs (- value1 value2)))

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
