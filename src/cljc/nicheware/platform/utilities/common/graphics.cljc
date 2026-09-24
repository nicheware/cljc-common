(ns nicheware.platform.utilities.common.graphics
"
Functions useful in graphics and drawing calculations.

There are groups of functions within the graphics namespace that deal with:

|Function group|Functions|
|---|---|
|size fitting| [[aspect-ratio]], [[fit-to-height]], [[fit-to-width]], [[fit-to-dimensions]], [[position-to-dimensions]], [[reverse-transform]]|
|shapes| [[update-fill-colors]], [[translate-shapes]]|
|curve conversion| [[arc->cubics]], [[ellipse->cubics]]|
|affine transforms| [[identity-matrix]], [[apply-matrix]], [[compose-matrix]], [[translate-matrix]], [[scale-matrix]], [[rotate-matrix]], [[transform->matrix]], [[invert-matrix]]|
|bounding boxes| [[bounding-box]], [[segments-bounding-box]], [[bounding-box-union]], [[transform-bounding-box]]|
|hatching| [[hatch-rect]]|

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
[:close]                      ; accepted by segments-bounding-box
```

**matrix** (affine transforms): a 6-vector ```[a b c d e f]``` in the SVG ```matrix(a,b,c,d,e,f)``` / PDF ```cm```
convention, mapping ```x' = a·x + c·y + e```, ```y' = b·x + d·y + f```. Any numbers are accepted; results are doubles.
[[compose-matrix]] follows SVG transform-list order (```(compose-matrix A B)``` applies B first), so the same
matrix can be written to an SVG ```transform``` attribute or a PDF ```cm``` operator.

```clojure
[1.0 0.0 0.0 1.0 0.0 0.0]   ; identity-matrix
```

**bounding box**: x/y are the minimum corner (top left in y-down coordinates). Functions return nil for
empty input.

```clojure
{:x 10.0
 :y 20.0
 :width 100.0
 :height 30.0}
```

Examples of transforms, bounding boxes and hatching:

```clojure
;; SVG transform=\"translate(10 0) scale(2) rotate(90 1 0)\"
(apply-matrix (transform->matrix {:translate [10 0] :scale 2 :rotate {:degrees 90 :about [1 0]}}) [2 0])
=>
[12.0 2.0]    ; within floating point noise

(segments-bounding-box [[:move [0 0]] [:cubic [0 10] [10 10] [10 0]]])
=>
{:x 0.0 :y 0.0 :width 10.0 :height 7.5}

(hatch-rect {:x 0 :y 0 :width 10 :height 5} {:pattern :horizontal :pitch 2})
=>
{:lines [[[0.0 0.0] [10.0 0.0]] [[0.0 2.0] [10.0 2.0]] [[0.0 4.0] [10.0 4.0]]] :dots []}
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


;; ======================== Affine transform functions =================================

(def identity-matrix
  "The identity affine matrix ```[1.0 0.0 0.0 1.0 0.0 0.0]```.

   Held as doubles, like every matrix these functions return, so it compares equal (with ```=```) to a
   computed identity such as ```(compose-matrix)``` on both Clojure and ClojureScript."
  [1.0 0.0 0.0 1.0 0.0 0.0])

(def ^:no-doc singular-determinant-tolerance
  "Matrices whose determinant has a smaller absolute value are treated as singular (not invertible)."
  1e-12)

(def ^:no-doc legacy-transform-keys
  "Keys that only appear in the ```{:x :y :scale}``` transform form."
  #{:x :y})

(def ^:no-doc component-transform-keys
  "Keys that only appear in the ```{:translate :scale :rotate}``` transform form."
  #{:translate :rotate})

(defn ^:no-doc validate-matrix
  "Throws an illegal argument exception unless m is a sequence of six finite numbers."
  [arg-name m]
  (when-not (and (sequential? m)
                 (= 6 (count m))
                 (every? math/finite-number? m))
    (common/throw-illegal-arg (str arg-name " must be an [a b c d e f] matrix of six finite numbers"))))

(defn ^:no-doc to-matrix
  "Converts a six number matrix to a vector of doubles."
  [m]
  (mapv double m))

(defn ^:no-doc multiply-matrices
  "Matrix product m1·m2 of two (validated) double matrices: the result applies m2 first, then m1."
  [[a1 b1 c1 d1 e1 f1] [a2 b2 c2 d2 e2 f2]]
  [(+ (* a1 a2) (* c1 b2))
   (+ (* b1 a2) (* d1 b2))
   (+ (* a1 c2) (* c1 d2))
   (+ (* b1 c2) (* d1 d2))
   (+ (* a1 e2) (* c1 f2) e1)
   (+ (* b1 e2) (* d1 f2) f1)])

(defn apply-matrix
  "Applies the affine matrix m to a point.

   Matrices use the SVG/PDF convention ```[a b c d e f]``` (SVG ```matrix(a,b,c,d,e,f)```, PDF ```a b c d e f cm```):

   ```x' = a·x + c·y + e```, ```y' = b·x + d·y + f```

  - m: ```[a b c d e f]``` of any finite numbers.
  - point: ```[x y]``` of any finite numbers.
  - returns: transformed point ```[x' y']``` as doubles.
  - throws: illegal argument exception if m is not a six number matrix or point not a two number point.

```clojure
(apply-matrix [2 0 0 3 10 20] [1 1])
=>
[12.0 23.0]
```
"
  [m point]
  (validate-matrix "m" m)
  (validate-point "point" point)
  (let [[a b c d e f] (to-matrix m)
        [x y] (to-point point)]
    [(+ (* a x) (* c y) e)
     (+ (* b x) (* d y) f)]))

(defn compose-matrix
  "Composes affine matrices in SVG transform-list order: ```(compose-matrix m1 m2)``` is the matrix product
   m1·m2, which applies **m2 first, then m1**. So SVG ```transform=\"A B C\"``` is ```(compose-matrix A B C)```,
   and ```(apply-matrix (compose-matrix m1 m2) p)``` equals ```(apply-matrix m1 (apply-matrix m2 p))```.

  - no arguments: returns [[identity-matrix]].
  - m: single matrix, returned as doubles.
  - m1 m2 & more: matrices to compose, leftmost applied last.
  - returns: ```[a b c d e f]``` doubles.
  - throws: illegal argument exception if any argument is not a six number matrix.

```clojure
(compose-matrix (translate-matrix [10 0]) (scale-matrix 2))
=>
[2.0 0.0 0.0 2.0 10.0 0.0]
```
"
  ([] identity-matrix)
  ([m]
   (validate-matrix "m" m)
   (to-matrix m))
  ([m1 m2 & more]
   (reduce (fn [product m] (multiply-matrices product (compose-matrix m)))
           (compose-matrix m1)
           (cons m2 more))))

(defn translate-matrix
  "Matrix for a translation, as SVG ```translate(dx dy)```.

  - translation: ```[dx dy]``` of finite numbers.
  - returns: ```[1.0 0.0 0.0 1.0 dx dy]```.
  - throws: illegal argument exception if translation is not a two number point.
"
  [translation]
  (validate-point "translate" translation)
  (let [[dx dy] (to-point translation)]
    [1.0 0.0 0.0 1.0 dx dy]))

(defn scale-matrix
  "Matrix for a scale about the origin, as SVG ```scale(sx sy)```.

  - scale: a number (uniform scale) or ```[sx sy]```.
  - returns: ```[sx 0.0 0.0 sy 0.0 0.0]``` doubles.
  - throws: illegal argument exception if scale is neither a finite number nor a two number vector.
"
  [scale]
  (let [[sx sy] (if (number? scale)
                  (do (validate-number "scale" scale) [scale scale])
                  (do (validate-point "scale" scale) scale))]
    [(double sx) 0.0 0.0 (double sy) 0.0 0.0]))

(defn rotate-matrix
  "Matrix for a rotation by degrees, optionally about a point, as SVG ```rotate(degrees cx cy)```.

   The rotation is the standard ```[cos θ, sin θ, −sin θ, cos θ, 0, 0]```: positive degrees turn +x towards +y,
   which is **clockwise on screen in y-down coordinates** (SVG, and PDF after the usual y flip). Rotating
   about a point is ```T(about)·R(θ)·T(−about)```. Values of sin/cos carry normal floating point noise
   (eg 90 degrees gives ```6.1e-17``` rather than 0).

  - degrees: rotation angle in degrees.
  - about: ```[x y]``` centre of rotation. Default ```[0 0]```.
  - returns: ```[a b c d e f]``` doubles.
  - throws: illegal argument exception if degrees is not a finite number or about not a two number point.

```clojure
(apply-matrix (rotate-matrix 90) [1 0])
=>
[6.123233995736766E-17 1.0]
```
"
  ([degrees] (rotate-matrix degrees [0 0]))
  ([degrees about]
   (validate-number "degrees" degrees)
   (validate-point "about" about)
   (let [theta (degrees->radians degrees)
         cos-theta (Math/cos theta)
         sin-theta (Math/sin theta)
         [ax ay] (to-point about)]
     (compose-matrix (translate-matrix [ax ay])
                     [cos-theta sin-theta (- sin-theta) cos-theta 0.0 0.0]
                     (translate-matrix [(- ax) (- ay)])))))

(defn ^:no-doc legacy-transform->matrix
  "Matrix of the ```{:x :y :scale}``` transform: scale about the origin, then translate by x, y."
  [{:keys [x y scale] :or {x 0 y 0 scale 1}}]
  (validate-number "transform :x" x)
  (validate-number "transform :y" y)
  (validate-number "transform :scale" scale)
  (let [s (double scale)]
    [s 0.0 0.0 s (double x) (double y)]))

(defn ^:no-doc component-transform->matrix
  "Matrix of the ```{:translate :scale :rotate}``` transform: M = T · S · R_about (rotate applied first)."
  [{:keys [translate scale rotate] :or {translate [0 0] scale 1}}]
  (when-not (or (nil? rotate) (map? rotate))
    (common/throw-illegal-arg "transform :rotate must be a map {:degrees :about}"))
  (let [{:keys [degrees about] :or {about [0 0]}} rotate]
    (compose-matrix (translate-matrix translate)
                    (scale-matrix scale)
                    (if rotate (rotate-matrix degrees about) identity-matrix))))

(defn transform->matrix
  "Converts a transform map into an ```[a b c d e f]``` affine matrix. Two forms are accepted:

   1. The library's existing **transform** ```{:x :y :scale}``` (as returned by [[fit-to-dimensions]] and
      undone by [[reverse-transform]]): scale is a number, x/y a translation, missing keys default to
      0 (x, y) and 1 (scale). Gives ```[scale 0 0 scale x y]```.
   2. A component transform ```{:translate [dx dy] :scale s-or-[sx sy] :rotate {:degrees θ :about [x y]}}```,
      all keys optional (```:about``` defaults to ```[0 0]```), always applied in the fixed order
      **rotate, then scale, then translate**: ```M = T · S · R_about```. This is the same as the SVG attribute
      ```transform=\"translate(..) scale(..) rotate(θ x y)\"```. Positive degrees rotate clockwise on screen
      in y-down coordinates (see [[rotate-matrix]]).

   Other keys (eg ```:width```/```:height``` from [[position-to-dimensions]]) are ignored. A map with only
   ```:scale``` (a number) means the same in both forms.

  - transform: map in one of the forms above.
  - returns: ```[a b c d e f]``` doubles.
  - throws: illegal argument exception if transform is not a map, mixes ```:x```/```:y``` with
    ```:translate```/```:rotate``` or an ```[sx sy]``` scale, or has non-numeric or malformed values.

```clojure
(transform->matrix {:x 10 :y 20 :scale 2})
=>
[2.0 0.0 0.0 2.0 10.0 20.0]

;; rotate 90 degrees about [1 0], scale 2, then translate [10 0]
(apply-matrix (transform->matrix {:translate [10 0] :scale 2 :rotate {:degrees 90 :about [1 0]}}) [2 0])
=>
[12.0 2.0]    ; within floating point noise
```
"
  [transform]
  (when-not (map? transform)
    (common/throw-illegal-arg "transform must be a map"))
  (let [legacy? (some #(contains? transform %) legacy-transform-keys)
        component? (or (some #(contains? transform %) component-transform-keys)
                       (sequential? (:scale transform)))]
    (cond
      (and legacy? component?)
      (common/throw-illegal-arg "transform must not mix :x/:y with :translate/:rotate or a [sx sy] :scale")

      component?
      (component-transform->matrix transform)

      :else
      (legacy-transform->matrix transform))))

(defn invert-matrix
  "Inverse of an affine matrix, so that ```(compose-matrix m (invert-matrix m))``` is (approximately)
   [[identity-matrix]]. Useful for mapping back from a transformed space, or for conjugating a
   transform (eg a y-flip: ```(compose-matrix flip m (invert-matrix flip))```).

  - m: ```[a b c d e f]``` of finite numbers.
  - returns: inverse ```[a b c d e f]``` as doubles.
  - throws: illegal argument exception if m is not a six number matrix, or is singular
    (```|a·d − b·c| < 1e-12```).

```clojure
(invert-matrix [2 0 0 4 10 20])
=>
[0.5 0.0 0.0 0.25 -5.0 -5.0]
```
"
  [m]
  (validate-matrix "m" m)
  (let [[a b c d e f] (to-matrix m)
        det (- (* a d) (* b c))]
    (when (< (Math/abs det) singular-determinant-tolerance)
      (common/throw-illegal-arg "matrix is singular and cannot be inverted"))
    ;; adding 0.0 normalises any -0.0 to 0.0
    (mapv #(+ 0.0 %)
          [(/ d det)
           (/ (- b) det)
           (/ (- c) det)
           (/ a det)
           (/ (- (* c f) (* d e)) det)
           (/ (- (* b e) (* a f)) det)])))


;; ======================== Bounding box functions =================================

(def ^:no-doc degenerate-coefficient-tolerance
  "Relative size below which a derivative coefficient is treated as zero when solving for cubic extrema."
  1e-12)

(defn bounding-box
  "Axis-aligned bounding box of a collection of points.

  - points: sequence of ```[x y]``` points (any finite numbers).
  - returns: ```{:x :y :width :height}``` doubles, x/y the minimum corner (top left in y-down coordinates),
    or nil when points is nil or empty.
  - throws: illegal argument exception if any point is not a two number point.

```clojure
(bounding-box [[1 5] [4 2] [-1 3]])
=>
{:x -1.0 :y 2.0 :width 5.0 :height 3.0}
```
"
  [points]
  (when (seq points)
    (doseq [point points] (validate-point "point" point))
    (let [xs (map #(double (first %)) points)
          ys (map #(double (second %)) points)
          min-x (apply min xs)
          min-y (apply min ys)]
      {:x min-x
       :y min-y
       :width (- (apply max xs) min-x)
       :height (- (apply max ys) min-y)})))

(defn ^:no-doc box-corners
  "The four corners of a bounding box, as [x y] points."
  [{:keys [x y width height]}]
  (let [right (+ x width)
        bottom (+ y height)]
    [[x y] [right y] [x bottom] [right bottom]]))

(defn ^:no-doc validate-box
  "Throws an illegal argument exception unless box is a map of finite :x :y and non-negative :width :height."
  [arg-name {:keys [x y width height] :as box}]
  (when-not (and (map? box)
                 (every? math/finite-number? [x y width height])
                 (not (neg? width))
                 (not (neg? height)))
    (common/throw-illegal-arg
     (str arg-name " must be a {:x :y :width :height} box of finite numbers, with non-negative size"))))

(defn ^:no-doc cubic-coordinate
  "Value at t of the one dimensional cubic Bezier with control values p0 p1 p2 p3 (Bernstein form)."
  [p0 p1 p2 p3 t]
  (let [u (- 1.0 t)]
    (+ (* u u u p0)
       (* 3.0 u u t p1)
       (* 3.0 u t t p2)
       (* t t t p3))))

(defn ^:no-doc cubic-point
  "Point at t on the cubic Bezier start, c1, c2, end."
  [[x0 y0] [x1 y1] [x2 y2] [x3 y3] t]
  [(cubic-coordinate x0 x1 x2 x3 t)
   (cubic-coordinate y0 y1 y2 y3 t)])

(defn ^:no-doc quadratic-roots
  "Real roots of a·t² + b·t + c = 0. Coefficients negligible relative to the largest are treated as zero,
   degrading to the linear (b·t + c) or constant case (no roots). Uses the numerically stable
   q = −(b + sign(b)·√disc)/2 form."
  [a b c]
  (let [scale (max (Math/abs a) (Math/abs b) (Math/abs c))
        negligible? #(<= (Math/abs %) (* degenerate-coefficient-tolerance scale))]
    (cond
      (zero? scale) []
      (negligible? a) (if (negligible? b) [] [(/ (- c) b)])
      :else (let [disc (- (* b b) (* 4.0 a c))]
              (if (neg? disc)
                []
                (let [q (* -0.5 (+ b (if (neg? b) (- (Math/sqrt disc)) (Math/sqrt disc))))]
                  (if (zero? q)
                    [0.0]
                    [(/ q a) (/ c q)])))))))

(defn ^:no-doc cubic-extrema-parameters
  "Parameters t in (0, 1) where one coordinate of the cubic Bezier p0 p1 p2 p3 has zero derivative.
   The derivative / 3 is a·t² + b·t + c with a = −p0 + 3p1 − 3p2 + p3, b = 2(p0 − 2p1 + p2), c = p1 − p0."
  [p0 p1 p2 p3]
  (->> (quadratic-roots (+ (- p0) (* 3.0 p1) (* -3.0 p2) p3)
                        (* 2.0 (+ p0 (* -2.0 p1) p2))
                        (- p1 p0))
       (filter #(< 0.0 % 1.0))))

(defn ^:no-doc cubic-extent-points
  "Points that bound the cubic from start to end: its end points plus its exact interior x and y extrema."
  [start c1 c2 end]
  (let [axis-parameters (fn [axis] (cubic-extrema-parameters (start axis) (c1 axis) (c2 axis) (end axis)))]
    (into [start end]
          (map #(cubic-point start c1 c2 end %))
          (concat (axis-parameters 0) (axis-parameters 1)))))

(defn ^:no-doc path-extent-step
  "Reducer over path segments. State ```{:current :subpath-start :points}```; adds each segment's extent
   points and tracks the current point (a :close returns it to the subpath start, as in SVG)."
  [{:keys [current subpath-start] :as state} segment]
  (when-not (sequential? segment)
    (common/throw-illegal-arg "path segment must be a vector such as [:move [x y]]"))
  (let [[op & args] segment]
    (case op
      :move (let [[p] args]
              (validate-point ":move point" p)
              (let [p (to-point p)]
                (-> state
                    (assoc :current p :subpath-start p)
                    (update :points conj p))))
      :line (let [[p] args]
              (validate-point ":line point" p)
              (let [p (to-point p)]
                (-> state
                    (assoc :current p)
                    (update :subpath-start #(or % p))
                    (update :points conj p))))
      :cubic (let [[c1 c2 p] args]
               (when-not current
                 (common/throw-illegal-arg ":cubic segment has no current point (no preceding :move)"))
               (validate-point ":cubic control point 1" c1)
               (validate-point ":cubic control point 2" c2)
               (validate-point ":cubic end point" p)
               (let [p (to-point p)]
                 (-> state
                     (assoc :current p)
                     (update :points into (cubic-extent-points current (to-point c1) (to-point c2) p)))))
      :close (assoc state :current subpath-start)
      (common/throw-illegal-arg (str "unknown path segment operation " (pr-str op))))))

(defn segments-bounding-box
  "Exact axis-aligned bounding box of a path, given as path segments (see the namespace docs):
   ```[:move p]```, ```[:line p]```, ```[:cubic c1 c2 p]``` and ```[:close]```.

   - ```:move``` and ```:line``` contribute their point.
   - ```:cubic``` contributes its start (the current point), its end, and its interior extrema, found by
     solving the derivative quadratic for each axis over t in (0, 1). The result is the tight box of the
     curve itself, not the (usually larger) hull of its control points.
   - ```:close``` contributes nothing (its line runs back to a point already included) and makes the
     subpath start the current point again.

  - segments: sequence of path segments.
  - returns: ```{:x :y :width :height}``` doubles, or nil if there are no points (nil, empty, or only
    ```[:close]```).
  - throws: illegal argument exception for a ```:cubic``` with no current point (before any ```:move```),
    an unknown operation, or malformed points.

```clojure
;; control points reach y = 10, but the curve itself only reaches y = 7.5
(segments-bounding-box [[:move [0 0]] [:cubic [0 10] [10 10] [10 0]]])
=>
{:x 0.0 :y 0.0 :width 10.0 :height 7.5}
```
"
  [segments]
  (->> segments
       (reduce path-extent-step {:current nil :subpath-start nil :points []})
       :points
       bounding-box))

(defn bounding-box-union
  "Smallest bounding box containing all the given boxes; nil boxes are ignored.

  - boxes: sequence of ```{:x :y :width :height}``` boxes or nils.
  - returns: ```{:x :y :width :height}``` doubles, or nil if there are no non-nil boxes.
  - throws: illegal argument exception if a non-nil box is malformed.

```clojure
(bounding-box-union [{:x 0 :y 0 :width 2 :height 2} nil {:x 5 :y -1 :width 1 :height 1}])
=>
{:x 0.0 :y -1.0 :width 6.0 :height 3.0}
```
"
  [boxes]
  (let [boxes (remove nil? boxes)]
    (doseq [box boxes] (validate-box "box" box))
    (bounding-box (mapcat box-corners boxes))))

(defn transform-bounding-box
  "Axis-aligned bounding box of a box after transforming it by an affine matrix: the box of its four
   transformed corners.

   Exact for the transformed rectangle itself. Under rotation or skew it is conservative (possibly larger
   than needed) for the original contents, since the contents need not reach the rectangle's corners;
   for a tight box transform the contents' points or segments before boxing them.

  - m: ```[a b c d e f]``` matrix.
  - box: ```{:x :y :width :height}```, or nil.
  - returns: ```{:x :y :width :height}``` doubles, or nil when box is nil.
  - throws: illegal argument exception if m or box is malformed.

```clojure
(transform-bounding-box (transform->matrix {:x 10 :y 0 :scale 2}) {:x 1 :y 1 :width 3 :height 4})
=>
{:x 12.0 :y 2.0 :width 6.0 :height 8.0}
```
"
  [m box]
  (validate-matrix "m" m)
  (when box
    (validate-box "box" box)
    (bounding-box (map #(apply-matrix m %) (box-corners box)))))


;; ======================== Hatching functions =================================

(def ^:no-doc half-root-two (Math/sqrt 0.5))

(def ^:no-doc exact-line-frames
  "Exact {:direction :normal} unit vectors for line angles (degrees, mod 180) that are multiples of 45, so
   named and numeric patterns agree exactly and carry no trigonometric noise.
   direction = (cos θ, −sin θ), normal = (sin θ, cos θ), in y-down coordinates."
  (let [h half-root-two]
    {0.0 {:direction [1.0 0.0] :normal [0.0 1.0]}
     45.0 {:direction [h (- h)] :normal [h h]}
     90.0 {:direction [0.0 -1.0] :normal [1.0 0.0]}
     135.0 {:direction [(- h) (- h)] :normal [h (- h)]}}))

(def ^:no-doc pattern-angles
  "Line angles (degrees, on screen) of the named line hatch patterns."
  {:horizontal [0]
   :vertical [90]
   :diagonal-45 [45]
   :diagonal-135 [135]
   :cross [45 135]})

(def ^:no-doc max-parameter
  "Stands in for an unbounded line parameter."
  #?(:clj Double/MAX_VALUE :cljs js/Number.MAX_VALUE))

(def ^:no-doc degenerate-hatch-tolerance
  "Clipped hatch lines shorter than this fraction of the rectangle's width + height are dropped
   (a line that only touches a corner)."
  1e-12)

(defn ^:no-doc line-frame
  "Unit direction and normal of lines at angle-degrees measured on screen (counter-clockwise from +x, with
   y down): direction (cos θ, −sin θ), normal (sin θ, cos θ)."
  [angle-degrees]
  (let [angle (+ 0.0 (mod (double angle-degrees) 180.0))]   ; + 0.0 normalises -0.0 (eg from -180)
    (or (exact-line-frames angle)
        (let [theta (degrees->radians angle)
              cos-theta (Math/cos theta)
              sin-theta (Math/sin theta)]
          {:direction [cos-theta (- sin-theta)]
           :normal [sin-theta cos-theta]}))))

(defn ^:no-doc axis-clip
  "Parameter interval over which base + t·direction lies within [low, high] on one axis, as
   ```[[t-enter edge-enter] [t-exit edge-exit]]``` (edge is the boundary value reached, nil when unbounded),
   or nil if the line never lies within the slab."
  [base direction low high]
  (if (zero? direction)
    (when (<= low base high)
      [[(- max-parameter) nil] [max-parameter nil]])
    (let [t-low [(/ (- low base) direction) low]
          t-high [(/ (- high base) direction) high]]
      (if (pos? direction) [t-low t-high] [t-high t-low]))))

(defn ^:no-doc clip-point
  "Point at t on base + t·direction, with the coordinate on axis (0 or 1) set exactly to edge when non-nil."
  [[bx by] [dx dy] [t axis edge]]
  (cond-> [(+ bx (* t dx)) (+ by (* t dy))]
    edge (assoc axis edge)))

(defn ^:no-doc clip-line-to-rect
  "Clips the infinite line base + t·direction to the rectangle (closed form: parametric line against the
   four edges). Returns the segment ```[p q]``` with p before q (smaller x, then smaller y), or nil when
   the line misses the rectangle or only touches it in a (near) single point."
  [{:keys [x y width height]} base direction]
  (let [x-clip (axis-clip (base 0) (direction 0) x (+ x width))
        y-clip (axis-clip (base 1) (direction 1) y (+ y height))]
    (when (and x-clip y-clip)
      (let [[[tx-in ex-in] [tx-out ex-out]] x-clip
            [[ty-in ey-in] [ty-out ey-out]] y-clip
            enter (if (>= tx-in ty-in) [tx-in 0 ex-in] [ty-in 1 ey-in])
            exit (if (<= tx-out ty-out) [tx-out 0 ex-out] [ty-out 1 ey-out])]
        (when (> (- (first exit) (first enter)) (* degenerate-hatch-tolerance (+ width height)))
          (vec (sort [(clip-point base direction enter) (clip-point base direction exit)])))))))

(defn ^:no-doc offset-range
  "Candidate integer multiples k of pitch covering values from low to high, padded by one each side
   (exact membership is decided by the caller)."
  [low high pitch]
  (range (dec (long (Math/floor (/ low pitch))))
         (+ 2 (long (Math/ceil (/ high pitch))))))

(defn ^:no-doc hatch-lines
  "Lines at angle-degrees, spaced pitch apart perpendicularly and anchored so one passes through the
   origin, clipped to rect, in increasing perpendicular offset."
  [rect angle-degrees pitch]
  (let [{:keys [direction normal]} (line-frame angle-degrees)
        offsets (map (fn [[cx cy]] (+ (* cx (normal 0)) (* cy (normal 1)))) (box-corners rect))]
    (into []
          (keep (fn [k]
                  (let [offset (* k pitch)]
                    (clip-line-to-rect rect [(* offset (normal 0)) (* offset (normal 1))] direction))))
          (offset-range (apply min offsets) (apply max offsets) pitch))))

(defn ^:no-doc grid-coordinates
  "Multiples of pitch in the half-open interval [low, low + size), ascending, as doubles."
  [low size pitch]
  (let [high (+ low size)]
    (into []
          (comp (map #(* (double %) pitch))
                (filter #(and (<= low %) (< % high))))
          (offset-range low high pitch))))

(defn ^:no-doc hatch-dots
  "Grid points [i·pitch j·pitch] in the half-open rect [x, x+width) × [y, y+height), sorted by y then x."
  [{:keys [x y width height]} pitch]
  (let [xs (grid-coordinates x width pitch)]
    (vec (for [gy (grid-coordinates y height pitch)
               gx xs]
           [gx gy]))))

(defn hatch-rect
  "Hatching (parallel lines or a dot grid) for an axis-aligned rectangle, analytically clipped to it, so
   the result is exact and deterministic and needs no clip support from the renderer.

   Line angles are measured on screen, counter-clockwise from +x, in y-down coordinates:

   |pattern|lines|
   |---|---|
   |```:horizontal```| 0 degrees|
   |```:vertical```| 90 degrees|
   |```:diagonal-45```| 45 degrees: rising left to right on screen (direction ```(1, −1)``` in y-down coordinates)|
   |```:diagonal-135```| 135 degrees: falling left to right on screen (direction ```(1, 1)```)|
   |```:cross```| both diagonals: the ```:diagonal-45``` lines followed by the ```:diagonal-135``` lines|
   |a number| lines at that angle in degrees (so 0 = ```:horizontal```, 90 = ```:vertical```, exactly)|
   |```:dots```| grid points, no lines|

   Lines are the family of parallel lines ```pitch``` apart (perpendicular spacing), phase-anchored to the
   global origin: the line through ```[0 0]``` is always a member. Adjacent rectangles with the same pattern
   and pitch therefore join up seamlessly. Each line is clipped exactly to the (closed) rectangle; a line
   that only touches a corner is dropped. Endpoints are ordered smaller x first (then smaller y), and lines
   are in increasing perpendicular offset, so equal inputs give ```=``` results.

   Dots are the grid points ```[i·pitch j·pitch]``` (i, j integers) in the half-open area
   ```[x, x+width) × [y, y+height)```, so adjacent rectangles never duplicate a dot. Sorted by y then x.

  - rect: ```{:x :y :width :height}```, x/y the top left, width and height positive.
  - options: map of
    - pattern: one of the patterns above.
    - pitch: positive spacing between lines, or between dots.
  - returns: ```{:lines [[[x1 y1] [x2 y2]] ...] :dots [[x y] ...]}``` doubles, both keys always present
    (line patterns give ```:dots []```, ```:dots``` gives ```:lines []```).
  - throws: illegal argument exception for a malformed rect, non-positive width, height or pitch, or an
    unknown pattern.

```clojure
(hatch-rect {:x 0 :y 0 :width 10 :height 5} {:pattern :horizontal :pitch 2})
=>
{:lines [[[0.0 0.0] [10.0 0.0]] [[0.0 2.0] [10.0 2.0]] [[0.0 4.0] [10.0 4.0]]]
 :dots []}

(hatch-rect {:x 0 :y 0 :width 4 :height 2} {:pattern :dots :pitch 2})
=>
{:lines [] :dots [[0.0 0.0] [2.0 0.0]]}
```
"
  [rect {:keys [pattern pitch]}]
  (validate-box "rect" rect)
  (when-not (and (pos? (:width rect)) (pos? (:height rect)))
    (common/throw-illegal-arg "rect width and height must be positive"))
  (validate-number "pitch" pitch)
  (when-not (pos? pitch)
    (common/throw-illegal-arg "pitch must be positive"))
  (let [rect (into {} (map (fn [[k v]] [k (double v)])) (select-keys rect [:x :y :width :height]))
        pitch (double pitch)]
    (cond
      (= :dots pattern)
      {:lines [] :dots (hatch-dots rect pitch)}

      (or (contains? pattern-angles pattern) (math/finite-number? pattern))
      {:lines (into [] (mapcat #(hatch-lines rect % pitch)) (get pattern-angles pattern [pattern]))
       :dots []}

      :else
      (common/throw-illegal-arg (str "unknown hatch pattern " (pr-str pattern))))))
