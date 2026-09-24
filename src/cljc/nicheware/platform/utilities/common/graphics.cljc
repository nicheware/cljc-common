(ns nicheware.platform.utilities.common.graphics
"
Functions useful in graphics and drawing calculations.

There are groups of functions within the graphics namespace that deal with:

|Function group|Functions|
|---|---|
|size fitting| [[aspect-ratio]], [[fit-to-height]], [[fit-to-width]], [[fit-to-dimensions]], [[position-to-dimensions]], [[reverse-transform]]|
|shapes| [[update-fill-colors]], [[translate-shapes]]|
|curve conversion| [[arc->cubics]], [[ellipse->cubics]]|

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

**path segments** (curve conversion): points are ```[x y]``` doubles, in y-down (SVG) coordinates.

```clojure
[:move [x y]]
[:line [x y]]
[:cubic [c1x c1y] [c2x c2y] [x y]]
```

The curve conversion functions turn elliptical arcs and full ellipses into cubic Bezier segments, so
renderers without an arc operator (eg PDF) draw exactly the same curves as SVG:

```clojure
(arc->cubics {:from [88.0 52.0] :to [272.0 52.0] :rx 92.0 :ry 18.0 :large-arc? false :sweep? false})
=>
[[:cubic [88.0 61.94112549695428] [129.189803015567 70.0] [180.0 70.0]]
 [:cubic [230.810196984433 70.0] [272.0 61.94112549695428] [272.0 52.0]]]

(ellipse->cubics {:centre [0 0] :rx 2 :ry 1})
=>
[[:move [2.0 0.0]]
 [:cubic [2.0 0.5522847498307936] [1.1045694996615871 1.0] [0.0 1.0]]
 [:cubic [-1.1045694996615871 1.0] [-2.0 0.5522847498307936] [-2.0 0.0]]
 [:cubic [-2.0 -0.5522847498307936] [-1.1045694996615871 -1.0] [0.0 -1.0]]
 [:cubic [1.1045694996615871 -1.0] [2.0 -0.5522847498307936] [2.0 0.0]]]
```

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


;; ======================== Curve conversion functions =================================

(def ^:no-doc quarter-turn (/ Math/PI 2.0))

(def ^:no-doc ellipse-kappa
  "Control distance, on the unit circle, of a cubic approximating a 90 degree arc: 4/3·(√2−1)."
  0.5522847498307936)

(def ^:no-doc segment-count-tolerance
  "Slack (in quarter turns) so a sweep of exactly 90 degrees, with floating point noise, is one segment."
  1e-9)

(defn ^:no-doc validate-point
  "Throws an illegal argument exception unless point is a vector of two finite numbers."
  [arg-name point]
  (when-not (and (sequential? point)
                 (= 2 (count point))
                 (every? math/finite-number? point))
    (common/throw-illegal-arg (str arg-name " must be an [x y] point of two finite numbers"))))

(defn ^:no-doc validate-number
  "Throws an illegal argument exception unless value is a finite number."
  [arg-name value]
  (when-not (math/finite-number? value)
    (common/throw-illegal-arg (str arg-name " must be a finite number"))))

(defn ^:no-doc to-point
  "Converts an [x y] point of any numbers to a vector of doubles."
  [[x y]]
  [(double x) (double y)])

(defn ^:no-doc degrees->radians
  [degrees]
  (* (double degrees) (/ Math/PI 180.0)))

(defn ^:no-doc unit->ellipse-vector
  "Maps a vector in the ellipse's unit frame to drawing space: scales by rx/ry, then rotates by phi (radians).
   No translation, so this also maps direction (derivative) vectors."
  [{:keys [rx ry phi]} [ux uy]]
  (let [sx (* rx ux)
        sy (* ry uy)
        cos-phi (Math/cos phi)
        sin-phi (Math/sin phi)]
    [(- (* cos-phi sx) (* sin-phi sy))
     (+ (* sin-phi sx) (* cos-phi sy))]))

(defn ^:no-doc unit->ellipse-point
  "Maps a point in the ellipse's unit frame to drawing space: scale, rotate, then translate to the centre."
  [{[cx cy] :centre :as ellipse} unit-point]
  (let [[x y] (unit->ellipse-vector ellipse unit-point)]
    [(+ cx x) (+ cy y)]))

(defn ^:no-doc add-scaled
  "Returns point + scale · direction."
  [[px py] scale [dx dy]]
  [(+ px (* scale dx)) (+ py (* scale dy))])

(defn ^:no-doc unit-angle-point
  "Point on the unit circle at angle theta (radians)."
  [theta]
  [(Math/cos theta) (Math/sin theta)])

(defn ^:no-doc unit-angle-derivative
  "Derivative with respect to theta of the unit circle point at angle theta."
  [theta]
  [(- (Math/sin theta)) (Math/cos theta)])

(defn ^:no-doc vector-angle
  "Angle (radians, -π to π) of the vector [x y] from the positive x axis."
  [[x y]]
  (Math/atan2 y x))

(defn ^:no-doc sweep-angle
  "Signed sweep from angle start to angle end (radians), normalised per SVG F.6.5.6:
   in (-2π, 0] when sweep? is false, [0, 2π) when sweep? is true."
  [start end sweep?]
  (let [two-pi (* 2.0 Math/PI)
        raw (- end start)
        positive (let [m (mod raw two-pi)] (if (>= m two-pi) 0.0 m))]
    (if (or sweep? (zero? positive))
      positive
      (- positive two-pi))))

(defn ^:no-doc radii-correction
  "SVG F.6.6 step 3: the factor Λ = x1'²/rx² + y1'²/ry² used to scale up radii that are too small."
  [rx ry [x1p y1p]]
  (+ (/ (* x1p x1p) (* rx rx))
     (/ (* y1p y1p) (* ry ry))))

(defn ^:no-doc arc-centre-parameters
  "Converts an SVG endpoint-parameterised arc into centre parameterisation (SVG 1.1 F.6.5, with the F.6.6
   out-of-range radii correction). Assumes from ≠ to and non-zero radii.

  - arc: map ```{:from :to :rx :ry :x-rotation :large-arc? :sweep?}``` (from, to and radii as doubles,
    x-rotation in degrees).
  - returns: map ```{:centre [cx cy] :rx :ry :phi :theta1 :delta-theta}``` where rx/ry are the absolute,
    possibly scaled-up radii, phi is the x-rotation in radians and the angles are radians.
"
  [{[x1 y1] :from [x2 y2] :to :keys [rx ry x-rotation large-arc? sweep?]}]
  (let [phi (degrees->radians x-rotation)
        cos-phi (Math/cos phi)
        sin-phi (Math/sin phi)
        half-dx (/ (- x1 x2) 2.0)
        half-dy (/ (- y1 y2) 2.0)
        ;; Step 1: from point in the rotated frame centred on the chord midpoint
        x1p (+ (* cos-phi half-dx) (* sin-phi half-dy))
        y1p (+ (* (- sin-phi) half-dx) (* cos-phi half-dy))
        ;; F.6.6: absolute radii, scaled up uniformly if they cannot span the endpoints
        lambda (radii-correction (Math/abs rx) (Math/abs ry) [x1p y1p])
        scale (if (> lambda 1.0) (Math/sqrt lambda) 1.0)
        rx (* scale (Math/abs rx))
        ry (* scale (Math/abs ry))
        ;; Step 2: centre in the rotated frame
        rx2 (* rx rx)
        ry2 (* ry ry)
        denominator (+ (* rx2 y1p y1p) (* ry2 x1p x1p))
        numerator (- (* rx2 ry2) denominator)
        magnitude (Math/sqrt (max 0.0 (/ numerator denominator)))
        coefficient (if (= (boolean large-arc?) (boolean sweep?)) (- magnitude) magnitude)
        cxp (* coefficient (/ (* rx y1p) ry))
        cyp (* coefficient (- (/ (* ry x1p) rx)))
        ;; Step 3: centre in drawing space
        cx (+ (- (* cos-phi cxp) (* sin-phi cyp)) (/ (+ x1 x2) 2.0))
        cy (+ (+ (* sin-phi cxp) (* cos-phi cyp)) (/ (+ y1 y2) 2.0))
        ;; Step 4: start angle and sweep
        theta1 (vector-angle [(/ (- x1p cxp) rx) (/ (- y1p cyp) ry)])
        theta2 (vector-angle [(/ (- (- x1p) cxp) rx) (/ (- (- y1p) cyp) ry)])]
    {:centre [cx cy]
     :rx rx
     :ry ry
     :phi phi
     :theta1 theta1
     :delta-theta (sweep-angle theta1 theta2 sweep?)}))

(defn ^:no-doc arc-segment-count
  "Minimum number of equal segments, each at most 90 degrees, to cover the sweep delta-theta (radians)."
  [delta-theta]
  (max 1 (long (Math/ceil (- (/ (Math/abs delta-theta) quarter-turn) segment-count-tolerance)))))

(defn ^:no-doc arc-segment-cubic
  "The cubic approximating the ellipse arc from angle theta-a to theta-b (radians), starting at start-point
   and ending at end-point (both in drawing space, normally the ellipse points at those angles).

  - returns: ```[:cubic c1 c2 end-point]```
"
  [ellipse theta-a theta-b start-point end-point]
  (let [k (* (/ 4.0 3.0) (Math/tan (/ (- theta-b theta-a) 4.0)))
        start-tangent (unit->ellipse-vector ellipse (unit-angle-derivative theta-a))
        end-tangent (unit->ellipse-vector ellipse (unit-angle-derivative theta-b))]
    [:cubic
     (add-scaled start-point k start-tangent)
     (add-scaled end-point (- k) end-tangent)
     end-point]))

(defn ^:no-doc centre-arc->cubics
  "Converts a centre-parameterised arc (as returned by [[arc-centre-parameters]]) into cubics, from
   from-point to to-point. Intermediate end points are computed on the ellipse; the last is to-point exactly."
  [{:keys [theta1 delta-theta] :as ellipse} from-point to-point]
  (let [n (arc-segment-count delta-theta)
        step (/ delta-theta n)
        angles (mapv #(+ theta1 (* step %)) (range (inc n)))
        end-points (conj (mapv #(unit->ellipse-point ellipse (unit-angle-point %)) (subvec angles 1 n))
                         to-point)
        start-points (into [from-point] (butlast end-points))]
    (mapv arc-segment-cubic
          (repeat ellipse) angles (rest angles) start-points end-points)))

(defn arc->cubics
  "Converts an SVG elliptical arc (endpoint parameterisation, exactly as the SVG path ```A``` command) into
   cubic Bezier segments, so the same curve can be drawn by renderers without an arc operator (eg PDF).

   Follows SVG 1.1 implementation notes F.6.5 (endpoint to centre conversion) and F.6.6 (out-of-range radii),
   including the F.6.2 edge cases:

  - from equal to to: returns ```[]``` (the arc is omitted).
  - rx or ry zero: returns ```[[:line to]]```.
  - negative radii: their absolute values are used.
  - radii too small to span the endpoints: scaled up uniformly until they just do (the arc is then a
    half ellipse).

   The sweep is split into the minimum number of equal segments each of at most 90 degrees (so at most 4).
   Each segment uses the control distance ```k = 4/3·tan(Δθ/4)``` on the unit circle, then scaled by rx/ry,
   rotated by x-rotation and translated to the centre. The radial error is under 0.03% of the radius.

   Coordinates follow SVG semantics: in a y-down system sweep-flag true (\"positive-angle\" direction) is
   clockwise on screen, false is counter-clockwise on screen.

   Arguments are a single map:

  - from: ```[x y]``` start point (the caller's current point; no ```:move``` is emitted).
  - to: ```[x y]``` end point.
  - rx: x radius.
  - ry: y radius.
  - x-rotation: rotation of the ellipse x axis, in degrees. Default 0.
  - large-arc?: SVG large-arc-flag. Default false.
  - sweep?: SVG sweep-flag. Default false.
  - returns: vector of ```[:cubic c1 c2 p]``` segments (points are ```[x y]``` doubles), the last ending
    exactly at to. Any numbers (Long, Ratio, Double) are accepted; computation is in doubles.
  - throws: illegal argument exception if from/to are not two-number points or rx, ry or x-rotation are not
    finite numbers.

  eg: a half ellipse, centre ```[180 52]```, drawn counter-clockwise on screen (y-down), passing below the
  chord through ```[180 70]```:
```clojure
(arc->cubics {:from [88.0 52.0] :to [272.0 52.0] :rx 92.0 :ry 18.0
              :x-rotation 0 :large-arc? false :sweep? false})
=>
[[:cubic [88.0 61.94112549695428] [129.189803015567 70.0] [180.0 70.0]]
 [:cubic [230.810196984433 70.0] [272.0 61.94112549695428] [272.0 52.0]]]
```
"
  [{:keys [from to rx ry x-rotation large-arc? sweep?] :or {x-rotation 0}}]
  (validate-point "from" from)
  (validate-point "to" to)
  (validate-number "rx" rx)
  (validate-number "ry" ry)
  (validate-number "x-rotation" x-rotation)
  (let [from-point (to-point from)
        to-point (to-point to)]
    (cond
      (= from-point to-point) []
      (or (zero? rx) (zero? ry)) [[:line to-point]]
      :else (-> {:from from-point
                 :to to-point
                 :rx (double rx)
                 :ry (double ry)
                 :x-rotation x-rotation
                 :large-arc? large-arc?
                 :sweep? sweep?}
                arc-centre-parameters
                (centre-arc->cubics from-point to-point)))))

(def ^:no-doc unit-quarter-cubics
  "The four quarter cubics of the unit circle, from angle 0 in the positive-angle direction, as
   ```[c1 c2 p]``` in the unit frame. Exact values so the quadrant points have no trigonometric noise."
  (let [k ellipse-kappa]
    [[[1.0 k] [k 1.0] [0.0 1.0]]
     [[(- k) 1.0] [-1.0 k] [-1.0 0.0]]
     [[-1.0 (- k)] [(- k) -1.0] [0.0 -1.0]]
     [[k -1.0] [1.0 (- k)] [1.0 0.0]]]))

(defn ellipse->cubics
  "Converts a full (optionally rotated) ellipse into a path of four quarter cubic Bezier segments.

   Starts at the point at angle 0 (```[(+ cx rx) cy]``` when unrotated) and goes in the positive-angle
   direction, returning exactly to the start point. In y-down coordinates (SVG) that is towards +y first,
   ie clockwise on screen. Each quarter uses the control distance κ = 4/3·(√2−1) = 0.5522847498307936,
   giving a radial error under 0.03%. The path is not closed: append ```[:close]``` if wanted.
   A circle is ```rx = ry```.

  - centre: ```[cx cy]``` centre of the ellipse.
  - rx: x radius, must be positive.
  - ry: y radius, must be positive.
  - x-rotation: rotation of the ellipse x axis, in degrees. Default 0.
  - returns: ```[[:move p0] [:cubic c1 c2 p1] [:cubic ...] [:cubic ...] [:cubic c1 c2 p0]]```, points as
    ```[x y]``` doubles.
  - throws: illegal argument exception if centre is not a two-number point, rx or ry are not positive
    finite numbers, or x-rotation is not a finite number.

  eg:
```clojure
(ellipse->cubics {:centre [0 0] :rx 2 :ry 1})
=>
[[:move [2.0 0.0]]
 [:cubic [2.0 0.5522847498307936] [1.1045694996615871 1.0] [0.0 1.0]]
 [:cubic [-1.1045694996615871 1.0] [-2.0 0.5522847498307936] [-2.0 0.0]]
 [:cubic [-2.0 -0.5522847498307936] [-1.1045694996615871 -1.0] [0.0 -1.0]]
 [:cubic [1.1045694996615871 -1.0] [2.0 -0.5522847498307936] [2.0 0.0]]]
```
"
  [{:keys [centre rx ry x-rotation] :or {x-rotation 0}}]
  (validate-point "centre" centre)
  (validate-number "rx" rx)
  (validate-number "ry" ry)
  (validate-number "x-rotation" x-rotation)
  (when-not (and (pos? rx) (pos? ry))
    (common/throw-illegal-arg "rx and ry must be positive"))
  (let [ellipse {:centre (to-point centre)
                 :rx (double rx)
                 :ry (double ry)
                 :phi (degrees->radians x-rotation)}
        to-drawing (partial unit->ellipse-point ellipse)
        start-point (to-drawing [1.0 0.0])]
    (into [[:move start-point]]
          (map (fn [unit-cubic] (into [:cubic] (map to-drawing) unit-cubic)))
          unit-quarter-cubics)))
