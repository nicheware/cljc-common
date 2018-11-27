(ns nicheware.platform.utilities.common.graphics.interpolate
  (:require [nicheware.platform.utilities.common.graphics.line :as line]
            [nicheware.platform.utilities.common.core :as common]))

;; ======================================== Constants ====================================

(def default-ease 0.42)
(def default-step-fraction 0.05)
(def interpolation-types [:linear :ease-in :ease-out :ease-in-out :cubic-bezier :quadratic-bezier :step-up :step-down])

;; ====================================== Functions for dealing with options ================

(defn get-options
  "Get options for use in interpolation by merging defaults with given options."
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

(defn select-dimensions
  "Given a set of points in which all dimensions have been interpolated, modify them so that
   only those dimensions selected in the :active-dimensions remain, and the rest default to the start dimension."
  [points start options]
  (if-let [active-dimensions (:active-dimensions options)]
    (map  #(common/selective-merge start % active-dimensions) points)
    points))

;; ====================================== Interpolate multi method ============================

;; Works with the following data
;;
;; {:type :linear | :ease-in | :ease-out | :ease-in-out | :cubic-bezier | :quadratic-bezier | :step-up | :step-down
;;  :control1 [...]  - Control point 1 (eg quadratic-bezier and cubic-bezier)
;;  :control2 [...]  - Control point 2 (eg cubic-bezier)
;;  :ease <0 to 0.5> - ratio of easing. Defaults to 0.42
;;  :step { :fraction <0 to 1>  - What fraction of range for each co-ord to step up or down
;;          :ranges [x x x x]}  - Range of each dimension incoming points,
;;                                used to calculate a different step per dimension
;;  :active-dimensions [true|failse true|false ...] - Indicate which dimension to modify during interpolation
;;                                                    Any not modified will use the value of the start for that dimension.
;;}

(defmulti interpolate
  "Performs interpolation between the two given points, create the specified
   number of interleaving points. The type of interpolation is determined by the
   options argument, of which the :type field will trigger different interpolation."
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
