(ns nicheware.platform.utilities.common.graphics.line
"
  Functions for with mathematical straight and curved lines:

There are groups of functions and variables within line that deal with:

  - equations: [[straight-line-equation]], [[bezier-qudaratic-equation]], [[bezier-cubic-equation]]

  - curve and line building functions: [[make-lerp]], [[lerp]], [[make-curve-fn-from-sample]], [[rasterize-bezier-quadratic]], [[interpolate-n-points]]

"
(:require [nicheware.platform.utilities.common.core :as common]))


;; ========================== Equations ===========================================

(defn make-lerp
  "Create a function that
   calculates the co-ordinate [x y, ...] that is fraction between the two given co-ordinates.

  - fraction is a float from 0 to 1.0.
  - returns: point co-ords will be floats"
  [point1 point2]
  (let [difference (map #(- %2 %1) point1 point2)]
    (fn [fraction]
      (map #(+ %1 (* %2 fraction)) point1 difference))))

(defn lerp
  "Calculates the point [x y ...] that is fraction between the two given points (of any dimension).

   - fraction is a float from 0 to 1.0.
   - returns: point co-ords will be floats and the same dimension as the incoming points.

  Does multi-dimension vector calculations, not just 2-D points."
  [point1 point2 fraction]
  (let [difference (map #(- %2 %1) point1 point2)]
    (map #(+ %1 (* %2 fraction)) point1 difference)))

(defn straight-line-equation
  "Given two points will create a line equation function.

   The equation function
   will take any X and return the Y corresponding to the point on the line.

   The result will be a float"
  [[x1 y1 :as  start] [x2 y2 :as  end]]
  (let [slope (/ (- y2 y1) (- x2 x1))
        constant (- y2 (* slope x2))]
    (fn [x] (float (+ constant (* slope x))))))

(defn bezier-quadratic-equation
  "Creates a function for computing points on a quadratic Bezier curve (from 3 control points)

   The returned function will accept a fraction from 0 to 1 which will return
   a point on the curve running from start to end, controlled by the middle.

   The middle point is used to calculate a line between the start and end point.
   The fraction then compute a point along each of these lines, which will be the
   tangent to the point on the curve. The point is the fraction of this resulting line."
  [start middle end]
  (let [start-lerp-fn (make-lerp start middle)
        end-lerp-fn (make-lerp middle end)]
    (fn [fraction]
      (let [start-point (start-lerp-fn fraction)
            end-point (end-lerp-fn fraction)]
        (lerp start-point end-point fraction)))))

(defn bezier-cubic-equation
  "Creates a function for computing points on a quadratic Bezier curve (from 5 control points)
   The returned function will accept a fraction from 0 to 1 which will return
   a point on the curve running from start to end, controlled by the middle two control points.

   Lines are computed between:

   - start -> control1 (L1)
   - control1 -> control2 (L2)
   - control2 -> end (L3)

   t (fraction 0 to 1) will compute points on each line, resulting in T1, T2, T3

   - these are then used as a bezier-quadratic-equation.
   "
  [start control1 control2 end]
  (let [start-lerp-fn (make-lerp start control1)
        middle-lerp-fn (make-lerp control1 control2)
        end-lerp-fn (make-lerp control2 end)]
    (fn [fraction]
      (let [start-point (start-lerp-fn fraction)
            middle-point (middle-lerp-fn fraction)
            end-point (end-lerp-fn fraction)
            bezier-fn (bezier-quadratic-equation start-point middle-point end-point)]
        (bezier-fn fraction)))))


(defn make-curve-fn-from-samples
  "Creates a function to return a Y value given an X for points on a curve
  where the curve is represented by the given set of [x y] points

  Finds the two points either size of the given X and then finds points on line between the known points.

  Returns nil if no part of curve."
  [sample-points]
  (fn [x]
    (let [first-x (first (first sample-points))
          last-x (first (last sample-points))]
      ;; Check point within defined curve sample points
      (if (or (< x first-x) (> x last-x))
        nil

        ;; Find the sample point with the first x greater or equal to the point we want
        (let [x-limit-fn (fn [[px py] x] (>= px x))
              end-index (common/find-index sample-points x x-limit-fn)
              [end-x end-y :as end-point] (nth sample-points end-index)]

          ;; If the sample point is exactly the one we want
          ;; use the y from sample points
          (if (= end-x x)
            (float end-y)

            ;; Our x lies between two samples points, find the y position on the line
            ;; between the two sample points
            (let [start-point (nth sample-points (dec end-index))
                  line-fn (straight-line-equation start-point end-point)]
              (line-fn x))))))))

(defn rasterize-bezier-quadratic
  "Rasterizes the bezier curve defined by the given start, end and middle control point.

  Will return a value for each int x between the start x and end x.

  As curve points can extend
  beyond the start and end point, it allows for an optional -start and x-end to be specified
  for the rasterized curve.

  These default to the start and end x,"

  ([[x1 y1 :as start] [x2 y2 :as end]  middle]
   (rasterize-bezier-quadratic start end middle x1 x2))

  ([[x1 y1 :as start] [x2 y2 :as end] [x3 y3 :as middle] x-start x-end]
   (let [bezier-fn (bezier-quadratic-equation start middle end)
         oversample-multiplier 2
         t-step (float (/ 1 (* (- x-end x-start) oversample-multiplier)))
         t-range (concat (range 0 1 t-step) [1])
         sample-points (map bezier-fn t-range)
         x-to-y-fn (make-curve-fn-from-samples sample-points)
         points-fn (fn [x] [x (Math/round (x-to-y-fn x))])
         points (map points-fn (range x-start (inc x-end)))
         ]
     points)))

(defn interpolate-n-points
  "Compute the given number of points between start and end.
   This will divide the curve into points + 1 sections, and gives the curve value
   for n points.

   - interpolation-fn:   Curve interpolation function which should accept t, as value form 0 to 1
   - points:             An integer number of points to interpolate evenly using the function.
 "
  [interpolation-fn points]
  (let [step-size (/ 1 (inc points))
        t-points (map #(* step-size %) (range 1 (inc points)))]
    (println "interpolate-n-points(): step-size: " step-size " t-points: " t-points)
    (map interpolation-fn t-points)))
