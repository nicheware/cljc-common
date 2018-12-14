(ns nicheware.platform.utilities.common.graphics
"
Functions useful in graphics and drawing calculations.

There are groups of functions within graphics that deal with:

  - size fitting: eg [[aspect-ratio]], [[fit-to-height]], [[fit-to-width]], [[fit-to-dimensions]], [[position-to-dimensions]], [[reverse-transform]]

  - shapes: eg [[update-fill-colors]], [[translate-shapes]]

In these functions the follow map logical data types are supported:

 dimensions:

```clojure
{:width 100
 :height 20}
```

 shape:

```clojure
{:shape :rect
 :x 10
 :y 30
 :width 100
 :height 30
 :fill-color 3}
```

"
  (:require [nicheware.platform.utilities.common.core :as common]
            [nicheware.platform.utilities.common.math :as math]))


;; ================================= Size fitting functions =============================

(defn aspect-ratio
  "Computes the aspect ratio of the dimensions (represented as a map)"
  [{:keys [width height] :as dimensions}]
  (math/div width height))

(defn fit-to-height
  "Does a fit to height calculation of the dimensions-to-fit, within the containing-dimension.

  - containing-dimensions and dimensions-to-fit are both ```{:width :height}```

   Returns a map ```{:width :height :x :y :scale}``` where:

  - returned width and height will retain the aspect ratio of dimensions-to-fit,
  - height will equal the height of containing-dimensions
   - x will be translation to position centrally, and y is 0
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

  - containing-dimensions and dimensions-to-fit are both ```{:width :height}```

   Returns a map ```{:width :height :x :y :scale}``` where:

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

  - containing-dimensions and dimensions-to-fit are both ```{:width :height}```

   Returns a map ```{:width :height :scale}``` where:

  - returned width and height will retain the aspect ratio of dimensions-to-fit,
  - either returned width or height will equal the width or height of containing-dimensions
  - scale will be the scale factor to apply to width and height of dimensions-to-fit
    to get the new returned width and height.
  - x and y will centre either horizontally or vertically depending on the best fit.
"
  [containing-dimensions dimensions-to-fit]
  (if (< (aspect-ratio containing-dimensions) (aspect-ratio dimensions-to-fit))
    ;; Must fit to width
    (fit-to-width containing-dimensions dimensions-to-fit)
    (fit-to-height containing-dimensions dimensions-to-fit)))


(defn position-to-dimensions
  "Same calculation as fit-to-dimensions, but doesn't bother returning the new width and height of the shape being fit
   but instead the width and the height of the containing area, along with the x, y and scale needed to position
   the shape within this area.

   - containing-dimensions and dimensions-to-fit are both ```{:width :height}```
   - returns a map ```{:width :height :x :y :scale}```
"
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
  "Updates the fill color for every shape in a sequence of shapes.

   Shapes will typically have a number of attributes (eg :x, :y), but the only assumption
   of this function is that color is specified with the :fill-color key. eg:
```clojure
(update-fill-colors [{:shape :point :x 1 :y 1 :fill-color 2}
                     {:shape :square :x 10 :y 10 :width 10 :fill-color 4}] 5)
  =>
[{:shape :point :x 1 :y 1 :fill-color 5}
 {:shape :square :x 10 :y 10 :width 10 :fill-color 5}]
```
"
  [shapes fill-color]
  (map #(assoc % :fill-color fill-color) shapes))

(defn translate-shapes
  "Updates the position all shapes in the given sequence, translating the :x and :y locations by the given amounts.
```clojure
(translate-shapes [{:shape :point :x 1 :y 1 :fill-color 2}
                   {:shape :square :x 10 :y 10 :width 10 :fill-color 4}] 2 3)
  =>
[{:shape :point :x 3 :y 4 :fill-color 2}
 {:shape :square :x 12 :y 13 :width 10 :fill-color 4}]
```
"
  [shapes trans-x trans-y]
  (let [translate-fn (fn [shape] (-> shape
                                     (update :x + trans-x)
                                     (update :y + trans-y)))]
    (map translate-fn shapes)))
