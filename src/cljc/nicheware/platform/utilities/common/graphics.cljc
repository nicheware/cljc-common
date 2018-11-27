(ns nicheware.platform.utilities.common.graphics
  (:require [nicheware.platform.utilities.common.core :as common]
            [nicheware.platform.utilities.common.math :as math]))


;; ================================= Size fitting functions =============================

(defn aspect-ratio
  [{:keys [width height] :as dimensions}]
  (math/div width height))

(defn fit-to-height
  "Does a fit to height calculation of the dimensions-to-fit, within the containing-dimension.
   Returns a map {:width :height :scale} where:
  - returned width and height will retain the aspect ratio of dimensions-to-fit,
  - height will equal the height of containing-dimensions
  - scale will be the scale factor to apply to width and height of dimensions-to-fit
    to get the new returned width and height.
"
  [{:keys [width height] :as containing-dimensions} {fit-width :width fit-height :height :as dimensions-to-fit}]
  (let [scale (math/div height fit-height)
        new-width (math/mult fit-width scale)
        x (math/div (- width new-width) 2)]
    {:width (math/mult fit-width scale)
     :height height
     :x x
     :y 0
     :scale scale}))

(defn fit-to-width
  "Does a fit to width calculation of the dimensions-to-fit, within the containing-dimension.
   Returns a map {:width :height :scale} where:
  - returned width and height will retain the aspect ratio of dimensions-to-fit,
  - width will equal the width of containing-dimensions
  - x will be 0 as we fill the entire area
  - y will be half of the height difference, so that we center in the new area.
  - scale will be the scale factor to apply to width and height of dimensions-to-fit
    to get the new returned width and height.
"
  [{:keys [width height] :as containing-dimensions}
   {fit-width :width fit-height :height :as dimensions-to-fit}]
  (let [scale (math/div width fit-width)
        new-height (math/mult fit-height scale)
        y (math/div (- height new-height) 2)]
    {:width width
     :height new-height
     :x 0
     :y y
     :scale scale}))

(defn fit-to-dimensions
  "Does a fit to size calculation of the dimensions-to-fit, within the containing-dimension.
   Returns a map {:width :height :scale} where:
  - returned width and height will retain the aspect ratio of dimensions-to-fit,
  - either returned width or height will equal the width or height of containing-dimensions
  - scale will be the scale factor to apply to width and height of dimensions-to-fit
    to get the new returned width and height.
"
  [containing-dimensions dimensions-to-fit]
  (if (< (aspect-ratio containing-dimensions) (aspect-ratio dimensions-to-fit))
    ;; Must fit to width
    (fit-to-width containing-dimensions dimensions-to-fit)
    (fit-to-height containing-dimensions dimensions-to-fit)))


(defn position-to-dimensions
  "Same calculation as fit-to-dimensions, but doesn't bother returning the new width and height of the shape being fit
   but instead the width and the height of the containing area, along with the x, y and scale needed to position
   the shape within this area."
  [containing-dimensions dimensions-to-fit]
  (merge (fit-to-dimensions containing-dimensions dimensions-to-fit)
         containing-dimensions))

(defn reverse-transform
  "Performs a reverse transformation of the co-ordinates, that have
   previously been transform according to the x, y translate and scale defined in dimensions.

   Useful for converting mouse co-ordinates in a transform co-ordinate space to model co-ordinates"
  [x y {:keys [scale] trans-x :x trans-y :y  :as dimensions}]
  {:x (/ (- x trans-x) scale)
   :y (/ (- y trans-y) scale)})


;; ======================== Shape functions =================================

;; TODO -= move to draw - this is part of the draw specification.

(defn update-fill-colors
  "Updates the fill color in a sequence of shapes. Only assumption is that color is :fill-color"
  [shapes fill-color]
  (map #(assoc % :fill-color fill-color) shapes))

(defn translate-shapes
  "Updates all shapes in the given sequence, translating the x and y locations by the given amounts."
  [shapes trans-x trans-y]
  (let [translate-fn (fn [shape] (-> shape
                                     (update :x + trans-x)
                                     (update :y + trans-y)))]
    (map translate-fn shapes)))
