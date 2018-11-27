(ns nicheware.platform.utilities.common.math)

;; ======================== Value manipulations  ================================


(defn roundn
  [precision num]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* num factor)) factor)))

(defn round
  [value]
  (Math/round (float value)))

(defn ceil
  "Ceiling call which converts to int to simplify later type checking"
  [value]
  (int (Math/ceil value)))

(defn floor
  "Ceiling call which converts to int to simplify later type checking"
  [value]
  (int (Math/floor value)))

(defn div
  "Do division in format consistent between clojurescript (which has no ratio) and clojure"
  [numer denom]
  (roundn 4 (/ numer denom)))

(defn diff
  "Absolute difference"
  [value1 value2]
  (Math/abs (- value1 value2)))

(defn mult
  "Multiple to 4 demical places"
  [arg1 arg2]
  (roundn 4 (* arg1 arg2)))

(defn clamp-change
  "Clamps the change (positive or negative) to the specified absolute magnitude"
  [start end max-abs-change]
  (let [change (- end start)]
    (if (< change 0)
      (+ start (max change (- 0 max-abs-change)))
      (+ start (min change max-abs-change)))))

(defn max-abs
  "Return whichever value is the biggest absolute value, retaining the sign on the return value.
  max-abs(-1 -2) => -2
  max-abs(1 2)   => 2
  max-abs(1 -2)  => -2"
  [val1 val2]
  (if (> (Math/abs val1) (Math/abs val2))
    val1
    val2))


;; ======================== Sequence generators =================================

(defn make-ratio-sequence-fn
  "Creates a function which accepts arguments 0, 1, ....n
   generating a sequence where each value is a progressive ratio of the supplied total.

   eg ratio = 0.5, total 10, then
   -initial point is half of total (0.5)
   -next point is half of remaining distance (7.5)
   -next os half again of remaining (8.75) etc

  f(x) = f(x-1) + ratio * (total - f(x-1)) "
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
   starting at start and getting closer to end (but never reaching)"
  [ratio start end]
  (let [sequence-fn (make-ratio-sequence-fn ratio (- end start))]
    (map #(+ start (sequence-fn %)) (range))))
