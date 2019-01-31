(ns nicheware.platform.utilities.common.graphics
"
Functions useful in graphics and drawing calculations.

There are groups of functions within the graphics namespace that deal with:

|Function group|Functions|
|---|---|
|size fitting| [[aspect-ratio]], [[fit-to-height]], [[fit-to-width]], [[fit-to-dimensions]], [[position-to-dimensions]], [[reverse-transform]]|
|shapes| [[update-fill-colors]], [[translate-shapes]]|

In these functions the following logical data types are supported:

 **dimensions**:

```clojure
{:width 100
 :height 20}
```

**transform**:

```clojure
{:x 10
 :y 20
 :scale 0.5}
```

 **shape**:

```clojure
{:shape :rect
 :x 10
 :y 30
 :width 100
 :height 30
 :fill-color 3}
```
The actual units and meaning of these fields is up to the code using these structures for drawing. These functions just update the values as requested.

"
  (:require [nicheware.platform.utilities.common.core :as common]
            [nicheware.platform.utilities.common.math :as math]))


;; ================================= Size fitting functions =============================

(defn aspect-ratio
  "Computes the aspect ratio of the dimensions (represented as a map).

  - dimensions: Dimension to calculate aspect ratio for. ```{:width :height}```
  - returns: aspect ratio, as a float.
"
  [{:keys [width height] :as dimensions}]
  (math/div width height))

(defn fit-to-height
  "Does a fit to height calculation of the dimensions-to-fit, within the containing-dimension.

  - containing-dimensions and dimensions-to-fit are both ```{:width :height}```
  - returns: a map ```{:width :height :x :y :scale}```

where:

  - width and height will retain the aspect ratio of dimensions-to-fit,
  - height will equal the height of containing-dimensions
  - x will be translation to position centrally, and y is 0
  - scale will be the scale factor to apply to width and height of dimensions-to-fit
    to get the new returned width and height.


  eg:
```clojure
(fit-to-height {:width 100 :height 100} {:width 10 :height 20})
=>
{:width 50.0, :height 100, :x 25.0, :y 0, :scale 5.0}
```


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

  - containing-dimensions: outer dimensions setting maximum width and height,  ```{:width :height}```
  - dimensions-to-fit: dimension defining the aspect ratio of the final dimension to fit inside containing-dimensions. ```{:width :height}```
  - returns: a map ```{:width :height :x :y :scale}```

where:

  - width and height will retain the aspect ratio of dimensions-to-fit,
  - width will equal the width of containing-dimensions
  - x will be 0 as we fill the entire area
  - y will ensure it is centered in the new area.
  - scale will be the scale factor to apply to width and height of dimensions-to-fit
    to get the new returned width and height.

  eg:
```clojure
(fit-to-width {:width 100 :height 100} {:width 10 :height 20})
=>
{:width 100, :height 200.0, :x 0, :y -50.0, :scale 10.0}
```

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

  - containing-dimensions: outer dimensions setting maximum width and height,  ```{:width :height}```
  - dimensions-to-fit: dimension defining the aspect ratio of the final dimension to fit inside containing-dimensions. ```{:width :height}```
  - returns: a map ```{:width :height :scale}```

where:

  - width and height will retain the aspect ratio of dimensions-to-fit,
  - either width or height will equal the width or height of containing-dimensions
  - x and y will centre either horizontally or vertically depending on the best fit.
  - scale will be the scale factor to apply to width and height of dimensions-to-fit
    to get the new returned width and height.

  eg:
```clojure
(fit-to-dimensions {:width 100 :height 100} {:width 10 :height 20})
=>
{:width 50.0, :height 100, :x 25.0, :y 0, :scale 5.0}
```
"
  [containing-dimensions dimensions-to-fit]
  (if (< (aspect-ratio containing-dimensions) (aspect-ratio dimensions-to-fit))
    ;; Must fit to  width
    (fit-to-width containing-dimensions dimensions-to-fit)
    (fit-to-height containing-dimensions dimensions-to-fit)))


(defn position-to-dimensions
  "Same calculation as [[fit-to-dimensions]], but doesn't bother returning the new width and height of the shape being fit
   but instead the width and the height of the containing area, along with the x, y and scale needed to position
   the shape within this area.

  - containing-dimensions: outer dimensions setting maximum width and height,  ```{:width :height}```
  - dimensions-to-fit: dimension defining the aspect ratio of the final dimension to fit inside containing-dimensions. ```{:width :height}```
  - returns a map ```{:width :height :x :y :scale}```

where:

  - width and height will be that of the containing-dimensions,
  - x and y will centre either horizontally or vertically depending on the best fit.
  - scale will be the scale factor to apply to width and height of dimensions-to-fit
    to fit the dimensions-to-fit within containing-dimensions

  eg:
```clojure
(position-to-dimensions {:width 100 :height 100} {:width 10 :height 20})
=>
{:width 100, :height 100, :x 25.0, :y 0, :scale 5.0}
```

"
  [containing-dimensions dimensions-to-fit]
  (merge (fit-to-dimensions containing-dimensions dimensions-to-fit)
         containing-dimensions))

(defn reverse-transform
  "Performs a reverse transformation of the co-ordinates, that have
   previously been transformed according to the x, y translate and scale defined in dimensions.

   Useful for converting mouse co-ordinates in a transform co-ordinate space to model co-ordinates.

   - x: X co-ordinate in source space.
   - y: Y co-ordinate in source space.
   - transform: Original transform applied to get the supplied x, y. ```{:scale :x :y}```
   - returns: x, y after applying reverse of transform to given x and y. ```{:x :y}```
"
  [x y {:keys [scale] trans-x :x trans-y :y  :as transform}]
  {:x (/ (- x trans-x) scale)
   :y (/ (- y trans-y) scale)})


;; ======================== Shape functions =================================

;; TODO -= move to draw - this is part of the draw specification.

(defn update-fill-colors
  "Updates the fill color for every shape in a collection of shapes.

   Shapes will typically have a number of attributes (eg ```:x, :y```), but the only assumption
   of this function is that color is specified with the ```:fill-color``` key.

  - shapes: Collection of shapes, where each shape is a map of the form: ```{:fill-color ...}```
  - fill-color: New fill color to set for each shape.
  - returns: Collection of shapes, where each shape has the supplied :fill-color.

eg:
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
  "Updates the position all shapes in the given collection, translating the :x and :y locations by the given amounts.

  - shapes: Collection of shapes, where each shape is a map of the form: ```{:fill-color ...}```
  - trans-x: Amount to transform each shape x value by.
  - trans-y: Amount to transform each shape y value by.
  - returns: Collection of shapes where all shape x and y have been translated as specified.

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
