(ns nicheware.platform.utilities.common.graphics.interpolate
"
Functions for performing different interpolations between two points.

|Function group|Functions|
|---|---|
|interpolation|[[get-options]], [[interpolate]]|

Interpolation is done between two n-dimension co-ordinates/points.

It can be used for calculating curves or lines in an n-dimensional spaces
or filling in a colour spectrum between two colours in a colour model.

The following interpolation types are supported:

|Type|Description|
|---|---|
|**linear**| direct linear interpolation|
|**quadratic-bezier**| a quadratic bezier interpolation between to two points and one control point.|
|**cubic-bezier**| a cubic bezier interpolation between the two points, using two control points.|
|**ease-in**| a quadratic bezier interpolation, where the control point is selected using the ease factor and end point.|
|**ease-out**| a quadratic bezier interpolation, where the control point is selected using the ease factor and end point.|
|**ease-in-out**| a cubic bezier interpolation, where the two control points are selected using the ease factors and end point.|
|**step-up**| computes a fix step up for each dimension.|
|**step-down**| computes a fixed step down for each dimension.|

 The variable [[interpolation-types]] is a vector of all the valid interpolation keywords.

"
  (:require [nicheware.platform.utilities.common.graphics.line :as line]
            [nicheware.platform.utilities.common.core :as common]))

;; ======================================== Constants ====================================

(def default-ease 0.42)
(def default-step-fraction 0.05)
(def interpolation-types [:linear :ease-in :ease-out :ease-in-out :cubic-bezier :quadratic-bezier :step-up :step-down])

;; ====================================== Functions for dealing with options ================

(defn get-options
  "Get options for use in interpolation by merging defaults with given options.

   - options: User provided options which will override (merge with) the default options.
   - start: 3 arity optional argument. Start control point for interpolation. vector of co-ordindates.
   - end: 3 arity optional argument. End control point for interpolation. vector of co-ordindates.
   - returns: map of options used for interpolation.

   Default options:
```clojure
{:type :linear
 :ease default-ease
 :control1 start
 :control2 end
 :step {:fraction default-step-fraction :ranges end}}
```

"
  ([options] (get-options options nil nil))
  ([options start end]
   (merge {:type :linear
           :ease default-ease
           :control1 start
           :control2 end

           :step {:fraction default-step-fraction
                  :ranges end}}
          options)))

;; ====================================== Utility function for pre/post processing ===========

(defn ^:no-doc select-dimensions
  "Given a set of points in which all dimensions have been interpolated, modify them so that
   only those dimensions selected in the :active-dimensions remain, and the rest default to the start dimension."
  [points start options]
  (if-let [active-dimensions (:active-dimensions options)]
    (map  #(common/selective-merge start % active-dimensions) points)
    points))

;; ====================================== Interpolate multi method ============================


(defmulti interpolate
  "Performs interpolation between the two given points, creating the specified
   number of interleaving points. The type of interpolation is determined by the
   options argument, of which the :type field will determine the type of interpolation used.

  - start: n-dimension vector marking start of transformation
  - end: n-dimension vector marking end of transformation
  - num-points: Number of interleaving points between start and end to return, using the interpolation function defined by options
  - options: Set of options defining attributes of the interpolation function to be used. See below
  - returns: vector of n-dimension vector, with each of the num-points entries being along the interpolation space.

  Options is of the form:

```clojure
 {:type :linear | :ease-in | :ease-out | :ease-in-out | :cubic-bezier | :quadratic-bezier | :step-up | :step-down
  :control1 [...]  - Control point 1 (eg quadratic-bezier and cubic-bezier)
  :control2 [...]  - Control point 2 (eg cubic-bezier)
  :ease <0 to 0.5> - ratio of easing. Defaults to 0.42
  :step { :fraction <0 to 1>  - What fraction of range for each co-ord to step up or down
          :ranges [x x x x]}  - Range of each dimension incoming points,
                                used to calculate a different step per dimension
  :active-dimensions [true|false  ...] - Indicate which dimension to modify during interpolation
                                         Any not modified will use the value of the start for that dimension.
}
```

  "
  (fn [start end num-points options] (:type options)))


;; Linear interpolation. Uses only :type from the options
(defmethod interpolate :linear
  [start end num-points options]
  (println "interpolate(:linear): num-points: " num-points)
  (-> (line/make-lerp start end)
      (line/interpolate-n-points num-points)
      (select-dimensions start options)))


;; Quadratic bezier interpolation. Uses :type and :control1 from the options
(defmethod interpolate :quadratic-bezier
  [start end num-points options]
  (let [{:keys [control1]} (get-options options start end)]
    (-> (line/bezier-quadratic-equation start control1 end)
        (line/interpolate-n-points num-points)
        (select-dimensions start options))))


;; Cubic bezier interpolation. Uses :type, :control1 and :control2 from the options
(defmethod interpolate :cubic-bezier
  [start end num-points options]
  (let [{:keys [control1 control2]} (get-options options start end)]
    (-> (line/bezier-cubic-equation start control1 control2 end)
        (line/interpolate-n-points num-points)
        (select-dimensions start options))))


;; Ease-in interpolation. Uses :type from options and creates a control1 then does quadratic interpolation
(defmethod interpolate :ease-in
  [start end num-points options]
  (let [ease-in (:ease (get-options options))
        control1 (map #(* ease-in %) end)]
    (-> (line/bezier-quadratic-equation start control1 end)
        (line/interpolate-n-points num-points)
        (select-dimensions start options))))

;; Ease-out interpolation. Uses :type from options and creates a control1 then does quadratic interpolation
(defmethod interpolate :ease-out
  [start end num-points options]
  (let [ease-out (- 1 (:ease (get-options options)))
        control1 (map #(* ease-out %) end)]
    (-> (line/bezier-quadratic-equation start control1 end)
        (line/interpolate-n-points num-points)
        (select-dimensions start options))))

;; Ease-in-out interpolation. Uses :type from options and creates a control1 and control2 then does cubic interpolation
(defmethod interpolate :ease-in-out
  [start end num-points options]
  (let [full-options (get-options options)
        ease-in (:ease full-options)
        ease-out (- 1 ease-in)
        control1 (map #(* ease-in %) end)
        control2 (map #(* ease-out %) end)]
    (-> (line/bezier-cubic-equation start control1 control2 end)
        (line/interpolate-n-points num-points)
        (select-dimensions start options))))


;; Step-up interpolation. Uses :step from options with fraction being combined with range to define
;; a different step for each dimension. Each dimension will max out at the top of the range
(defmethod interpolate :step-up
  [start end num-points options]
  (let [{{:keys [fraction ranges]} :step :as full-options} (get-options options start end)
        step-fraction (or fraction default-step-fraction)
        steps (map #(* step-fraction %) ranges)
        step-fn (fn [point] (map #(min (+ %1 %2) %3) point steps ranges))]
    (println "iterpolate:step-up(): step-fraction: " step-fraction " steps: " steps)
(-> (take (inc num-points) (iterate step-fn (step-fn start)))
        (select-dimensions start options))))


;; Step-up interpolation. Uses :step from options with fraction being combined with range to define
;; a different step for each dimension. Each dimension will max out at the top of the range
(defmethod interpolate :step-down
  [start end num-points options]
  (let [{{:keys [fraction ranges]} :step :as full-options} (get-options options end start)
        step-fraction (or fraction default-step-fraction)
        steps (map #(* step-fraction %) ranges)
        step-fn (fn [point] (map #(max (- %1 %2) 0) point steps))]
    (println "interpolate(:step-down): full-options: " full-options " steps: " steps " step-fn([0 0]): " (step-fn [0 0]))
    (-> (take (inc num-points) (iterate step-fn (step-fn start)))
        (select-dimensions start options))))
