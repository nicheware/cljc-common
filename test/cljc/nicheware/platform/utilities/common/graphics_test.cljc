(ns nicheware.platform.utilities.common.graphics-test
  (:require [nicheware.platform.utilities.common.graphics :as sut]
            [nicheware.platform.utilities.common.math :as math]
            [nicheware.platform.utilities.common.graphics.line :as line]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))


;; ================================= Size fitting functions =============================

(t/deftest test-aspect-ratio
  (t/testing "Test we can determine aspect ratio"
    (t/is (= 0.5
             (sut/aspect-ratio {:width 50 :height 100})))))

(t/deftest test-fit-to-height
  (t/testing "Small rect within larger rect, integer scale"
    (t/is (= {:width 20.0 :height 100 :x 40.0 :y 0 :scale 2.0}
             (sut/fit-to-height {:width 100 :height 100} {:width 10 :height 50}))))

  (t/testing "Small rect within larger rect, fraction scale"
    (t/is (= {:width 200.0004 :height 100 :x -50.0002 :y 0 :scale 16.6667}
             (sut/fit-to-height {:width 100 :height 100} {:width 12 :height 6}))))

  (t/testing "Large rect within smaller rect"
    (t/is (= {:width 10.0 :height 10 :x 0.0 :y 0 :scale 0.2}
             (sut/fit-to-height {:width 10 :height 10} {:width 50 :height 50})))))

(t/deftest test-fit-to-width
  (t/testing "Small rect within larger rect, integer scale"
    (t/is (= {:width 100 :height 20.0 :x 0 :y 40.0 :scale 2.0}
             (sut/fit-to-width {:width 100 :height 100} {:width 50 :height 10}))))

  (t/testing "Small rect within larger rect, fraction scale"
    (t/is (= {:width 100 :height 200.0004 :x 0 :y -50.0002 :scale 16.6667}
             (sut/fit-to-width {:width 100 :height 100} {:width 6 :height 12}))))

  (t/testing "Large rect within smaller rect"
    (t/is (= {:width 10 :height 10.0 :x 0 :y 0.0 :scale 0.2}
             (sut/fit-to-width {:width 10 :height 10} {:width 50 :height 50}))))

  (t/testing "Large rect within smaller rect of different aspect ratio"
    (t/is (= {:width 10 :height 10.0 :x 0 :y 5.0 :scale 0.2}
             (sut/fit-to-width {:width 10 :height 20} {:width 50 :height 50})))))

(t/deftest test-fit-to-dimensions
  (t/testing "Small rect within larger rect, integer scale"
    (t/is (= {:width 100 :height 20.0 :x 0 :y 40.0 :scale 2.0}
             (sut/fit-to-dimensions {:width 100 :height 100} {:width 50 :height 10}))))

  (t/testing "Small rect within larger rect, fraction scale"
    (t/is (= {:width 20.0 :height 100 :x 40.0 :y 0 :scale 2.0}
             (sut/fit-to-dimensions {:width 100 :height 100} {:width 10 :height 50}))))

  (t/testing "Large rect within smaller rect"
    (t/is (= {:width 10.0 :height 10 :x 0.0 :y 0 :scale 0.2}
             (sut/fit-to-dimensions {:width 10 :height 10} {:width 50 :height 50}))))

  (t/testing "Exmaple from size card- smaller then region but same aspect ratio"
    (t/is (= {:width 58.0 :height 58 :x 0.0 :y 0 :scale 29.0}
             (sut/fit-to-dimensions {:width 58 :height 58} {:width 2 :height 2})))))

(t/deftest test-position-to-dimensions
  (t/testing "Position within long rectangle"
    (t/is (= {:width 173, :height 58, :x 72.0, :y 0, :scale 29.0}
             (sut/position-to-dimensions {:width 173 :height 58} {:width 1 :height 2})))))


(t/deftest test-reverse-transform
  (t/testing "Test simple scale and translate, taken from logs"
    (t/is (= {:x 0.6424377491106995, :y 1.714310204431492}
             (sut/reverse-transform 177 4 {:x 175.501 :y 0 :scale 2.3333})))
    (t/is (= {:x 8.356833669052413, :y 1.714310204431492}
             (sut/reverse-transform 195 4 {:x 175.501 :y 0 :scale 2.3333})))))


;; ======================== Shape functions =================================


(t/deftest test-translate-shapes
  (t/testing "Simple x y translation check"
    (t/is (= [{:shape :point :x 14 :y 7}
              {:shape :point :x 24 :y 2}
              {:shape :point :x 34 :y -3}]
           (sut/translate-shapes [{:shape :point :x 10 :y 10}
                                  {:shape :point :x 20 :y 5}
                                  {:shape :point :x 30 :y 0}]
                                 4 -3)))))


;; ======================== Curve conversion functions =================================

(def radial-tolerance
  "Maximum allowed |r − 1| of a sampled cubic point in the ellipse's unit frame (0.03%)."
  0.0003)

(def tangent-tolerance 1e-6)

(def sample-count 64)

(defn- illegal-arg-thrown?
  [f]
  (try (f) false
       (catch #?(:clj IllegalArgumentException :cljs js/Error) _ true)))

(defn- to-radians [degrees] (* degrees (/ Math/PI 180.0)))

(defn- approx=
  ([a b] (approx= a b 1e-9))
  ([a b tolerance] (< (Math/abs (- a b)) tolerance)))

(defn- points-approx=
  ([p q] (points-approx= p q 1e-9))
  ([p q tolerance] (every? true? (map #(approx= %1 %2 tolerance) p q))))

(defn- normalise
  [[x y]]
  (let [length (Math/sqrt (+ (* x x) (* y y)))]
    [(/ x length) (/ y length)]))

(defn- ellipse-point
  "Point at angle theta-degrees on the ellipse {:centre :rx :ry :rotation} (rotation in degrees)."
  [{[cx cy] :centre :keys [rx ry rotation]} theta-degrees]
  (let [theta (to-radians theta-degrees)
        phi (to-radians rotation)
        x (* rx (Math/cos theta))
        y (* ry (Math/sin theta))]
    [(+ cx (- (* (Math/cos phi) x) (* (Math/sin phi) y)))
     (+ cy (+ (* (Math/sin phi) x) (* (Math/cos phi) y)))]))

(defn- ellipse-tangent
  "Derivative with respect to theta of the ellipse point at theta-degrees."
  [{:keys [rx ry rotation]} theta-degrees]
  (let [theta (to-radians theta-degrees)
        phi (to-radians rotation)
        x (- (* rx (Math/sin theta)))
        y (* ry (Math/cos theta))]
    [(- (* (Math/cos phi) x) (* (Math/sin phi) y))
     (+ (* (Math/sin phi) x) (* (Math/cos phi) y))]))

(defn- unit-radius
  "Maps point back into the ellipse's unit frame (un-translate, un-rotate, divide by rx/ry); returns its radius."
  [{[cx cy] :centre :keys [rx ry rotation]} [px py]]
  (let [phi (to-radians rotation)
        dx (- px cx)
        dy (- py cy)
        ux (/ (+ (* (Math/cos phi) dx) (* (Math/sin phi) dy)) rx)
        uy (/ (- (* (Math/cos phi) dy) (* (Math/sin phi) dx)) ry)]
    (Math/sqrt (+ (* ux ux) (* uy uy)))))

(defn- segment-starts
  "Start point of each cubic: the given first start, then each previous segment's end."
  [first-start cubics]
  (cons first-start (map last (butlast cubics))))

(defn- sample-cubics
  "Samples every cubic at sample-count parameter values, using bezier-cubic-equation."
  [first-start cubics]
  (mapcat (fn [start [_ c1 c2 end]]
            (let [curve (line/bezier-cubic-equation start c1 c2 end)]
              (map #(vec (curve (/ % (dec sample-count)))) (range sample-count))))
          (segment-starts first-start cubics)
          cubics))

(defn- max-radial-error
  [ellipse first-start cubics]
  (apply max (map #(Math/abs (- (unit-radius ellipse %) 1.0)) (sample-cubics first-start cubics))))

(defn- arc-case
  "Arc request for the arc of ellipse from angle start-degrees sweeping sweep-degrees (signed; positive is
   the positive-angle direction), with flags derived per SVG."
  [ellipse start-degrees sweep-degrees]
  {:from (ellipse-point ellipse start-degrees)
   :to (ellipse-point ellipse (+ start-degrees sweep-degrees))
   :rx (:rx ellipse)
   :ry (:ry ellipse)
   :x-rotation (:rotation ellipse)
   :large-arc? (> (Math/abs sweep-degrees) 180)
   :sweep? (pos? sweep-degrees)})

(def test-ellipses
  {:ellipse {:centre [100.0 50.0] :rx 40.0 :ry 15.0 :rotation 0}
   :circle {:centre [-20.0 30.0] :rx 25.0 :ry 25.0 :rotation 0}
   :rotated {:centre [10.0 -5.0] :rx 60.0 :ry 20.0 :rotation 30}
   :rotated-negative {:centre [0.0 0.0] :rx 7.0 :ry 3.0 :rotation -115}})

(def test-sweeps
  "[start-degrees sweep-degrees] pairs: small, 90, 180, 270 and large arcs, in both directions."
  [[10 20] [10 -20] [0 90] [45 -90] [0 180] [200 -180] [30 270] [30 -270]
   [-60 300] [100 -300] [5 181] [5 -181] [20 179] [20 -179] [0 359]])

(defn- expected-segment-count
  [sweep-degrees]
  (long (Math/ceil (- (/ (Math/abs sweep-degrees) 90.0) 1e-9))))

(def worked-example
  {:from [88.0 52.0] :to [272.0 52.0] :rx 92.0 :ry 18.0 :x-rotation 0 :large-arc? false :sweep? false})

(t/deftest test-arc->cubics-worked-example
  (let [cubics (sut/arc->cubics worked-example)
        ellipse {:centre [180.0 52.0] :rx 92.0 :ry 18.0 :rotation 0}
        samples (sample-cubics [88.0 52.0] cubics)]
    (t/testing "Half ellipse is two cubics ending exactly at to"
      (t/is (= 2 (count cubics)))
      (t/is (every? #(= :cubic (first %)) cubics))
      (t/is (= [272.0 52.0] (last (last cubics)))))

    (t/testing "Centre recovered as [180 52] with a -180 degree sweep"
      (let [{:keys [centre delta-theta]} (sut/arc-centre-parameters
                                          (assoc worked-example :x-rotation 0))]
        (t/is (points-approx= [180.0 52.0] centre))
        (t/is (approx= (- Math/PI) delta-theta))))

    (t/testing "sweep-flag 0 in y-down coordinates passes below the chord (y >= 52), through [180 70]"
      (t/is (points-approx= [180.0 70.0] (last (first cubics))))
      (t/is (every? (fn [[_ y]] (>= y (- 52.0 1e-9))) samples)))

    (t/testing "sweep-flag 1 passes above the chord (y <= 52), through [180 34]"
      (let [cubics (sut/arc->cubics (assoc worked-example :sweep? true))]
        (t/is (points-approx= [180.0 34.0] (last (first cubics))))
        (t/is (every? (fn [[_ y]] (<= y (+ 52.0 1e-9))) (sample-cubics [88.0 52.0] cubics)))))

    (t/testing "Within the approximation bound"
      (t/is (< (max-radial-error ellipse [88.0 52.0] cubics) radial-tolerance)))))

(t/deftest test-arc->cubics-geometry
  (doseq [[ellipse-name ellipse] test-ellipses
          [start-degrees sweep-degrees] test-sweeps]
    (let [{:keys [from to] :as arc} (arc-case ellipse start-degrees sweep-degrees)
          cubics (sut/arc->cubics arc)
          direction (if (pos? sweep-degrees) 1.0 -1.0)
          [_ first-c1] (first cubics)
          [_ _ last-c2 last-end] (last cubics)]
      (t/testing (str ellipse-name " start " start-degrees " sweep " sweep-degrees)
        (t/is (= (mapv double to) last-end) "Last end point is exactly to")
        (t/is (every? #(every? float? (mapcat identity (rest %))) cubics) "All coordinates are doubles")
        (t/is (= (map last (butlast cubics)) (rest (segment-starts from cubics))) "Segments are continuous")
        (t/is (= (expected-segment-count sweep-degrees) (count cubics)) "Minimum number of <= 90 degree segments")
        (t/is (<= (count cubics) 4))
        (t/is (< (max-radial-error ellipse from cubics) radial-tolerance) "Within 0.03% radial error")
        (t/is (points-approx= (normalise (map #(* direction %) (ellipse-tangent ellipse start-degrees)))
                              (normalise (map - first-c1 from))
                              tangent-tolerance)
              "Start tangent matches the ellipse")
        (t/is (points-approx= (normalise (map #(* direction %)
                                              (ellipse-tangent ellipse (+ start-degrees sweep-degrees))))
                              (normalise (map - last-end last-c2))
                              tangent-tolerance)
              "End tangent matches the ellipse")))))

(t/deftest test-arc->cubics-segment-count
  (let [circle {:centre [0.0 0.0] :rx 10.0 :ry 10.0 :rotation 0}
        count-for (fn [sweep-degrees] (count (sut/arc->cubics (arc-case circle 0 sweep-degrees))))]
    (t/testing "Exactly 90 degrees is a single segment"
      (t/is (= 1 (count-for 90)))
      (t/is (= 1 (count-for -90))))
    (t/testing "Just over a multiple of 90 needs another segment"
      (t/is (= 2 (count-for 90.01)))
      (t/is (= 3 (count-for 180.01)))
      (t/is (= 4 (count-for 270.01))))
    (t/testing "Counts for other sweeps"
      (t/is (= 1 (count-for 1)))
      (t/is (= 2 (count-for 180)))
      (t/is (= 3 (count-for -270)))
      (t/is (= 4 (count-for 359.9)))
      (t/is (= 4 (count-for -359.9))))))

(t/deftest test-arc->cubics-edge-cases
  (t/testing "Identical endpoints omit the arc"
    (t/is (= [] (sut/arc->cubics {:from [5 5] :to [5.0 5.0] :rx 10 :ry 10})))
    (t/is (= [] (sut/arc->cubics {:from [(/ 1 2) 0] :to [0.5 0] :rx 0 :ry 0}))))

  (t/testing "Zero radius is a straight line to the end point"
    (t/is (= [[:line [10.0 3.0]]] (sut/arc->cubics {:from [0 0] :to [10 3] :rx 0 :ry 5})))
    (t/is (= [[:line [10.0 3.0]]] (sut/arc->cubics {:from [0 0] :to [10 3] :rx 5 :ry 0.0}))))

  (t/testing "Negative radii are treated as their absolute values"
    (t/is (= (sut/arc->cubics worked-example)
             (sut/arc->cubics (assoc worked-example :rx -92.0 :ry -18.0))))
    (t/is (= (sut/arc->cubics worked-example)
             (sut/arc->cubics (assoc worked-example :ry -18.0)))))

  (t/testing "Radii too small are scaled up uniformly: circle"
    (let [cubics (sut/arc->cubics {:from [0 0] :to [10 0] :rx 1 :ry 1})
          ellipse {:centre [5.0 0.0] :rx 5.0 :ry 5.0 :rotation 0}]
      (t/is (= 2 (count cubics)))
      (t/is (= [10.0 0.0] (last (last cubics))))
      (t/is (points-approx= [5.0 5.0] (last (first cubics))))
      (t/is (< (max-radial-error ellipse [0.0 0.0] cubics) radial-tolerance))))

  (t/testing "Radii too small are scaled up uniformly, keeping the rx:ry ratio, with rotation"
    (let [ellipse {:centre [0.0 0.0] :rx 8.0 :ry 2.0 :rotation 40}
          from (ellipse-point ellipse 170)
          to (ellipse-point ellipse -10)
          cubics (sut/arc->cubics {:from from :to to :rx 4 :ry 1 :x-rotation 40 :sweep? true})
          {:keys [centre rx ry]} (sut/arc-centre-parameters {:from from :to to :rx 4.0 :ry 1.0
                                                             :x-rotation 40 :sweep? true})]
      (t/is (points-approx= [0.0 0.0] centre 1e-9))
      (t/is (approx= 8.0 rx))
      (t/is (approx= 2.0 ry))
      (t/is (= 2 (count cubics)))
      (t/is (< (max-radial-error ellipse from cubics) radial-tolerance))))

  (t/testing "Any numbers are accepted and computed as doubles"
    (t/is (= (sut/arc->cubics worked-example)
             (sut/arc->cubics {:from [88 52] :to [272 52] :rx 92 :ry 18 :x-rotation 0})))
    (t/is (= (sut/arc->cubics {:from [0.5 0.0] :to [10.5 0.0] :rx 2.5 :ry 5.0 :x-rotation 0.0})
             (sut/arc->cubics {:from [(/ 1 2) 0] :to [(/ 21 2) 0] :rx (/ 5 2) :ry 5 :x-rotation 0}))))

  (t/testing "x-rotation and flags default to 0 and false"
    (t/is (= (sut/arc->cubics worked-example)
             (sut/arc->cubics (dissoc worked-example :x-rotation :large-arc? :sweep?)))))

  (t/testing "Invalid arguments throw"
    (t/is (illegal-arg-thrown? #(sut/arc->cubics (assoc worked-example :from nil))))
    (t/is (illegal-arg-thrown? #(sut/arc->cubics (assoc worked-example :to [1]))))
    (t/is (illegal-arg-thrown? #(sut/arc->cubics (dissoc worked-example :rx))))
    (t/is (illegal-arg-thrown? #(sut/arc->cubics (assoc worked-example :x-rotation "0"))))))

(t/deftest test-ellipse->cubics
  (doseq [[ellipse-name {:keys [centre rx ry rotation] :as ellipse}] test-ellipses]
    (let [path (sut/ellipse->cubics {:centre centre :rx rx :ry ry :x-rotation rotation})
          [[move-op start] & cubics] path]
      (t/testing (str ellipse-name " is a move then four quarter cubics")
        (t/is (= 5 (count path)))
        (t/is (= :move move-op))
        (t/is (every? #(= :cubic (first %)) cubics)))
      (t/testing (str ellipse-name " starts at angle 0, goes positive-angle first and returns exactly")
        (t/is (points-approx= (ellipse-point ellipse 0) start))
        (t/is (points-approx= (ellipse-point ellipse 90) (last (first cubics))))
        (t/is (points-approx= (ellipse-point ellipse 180) (last (second cubics))))
        (t/is (= start (last (last cubics)))))
      (t/testing (str ellipse-name " within the approximation bound")
        (t/is (< (max-radial-error ellipse start cubics) radial-tolerance)))))

  (t/testing "Unrotated start is [cx+rx cy] and the first quarter heads towards +y (y-down: clockwise)"
    (let [[[_ start] [_ c1 _ end]] (sut/ellipse->cubics {:centre [1 2] :rx 3 :ry (/ 1 2)})]
      (t/is (= [4.0 2.0] start))
      (t/is (= [1.0 2.5] end))
      (t/is (> (second c1) 2.0))))

  (t/testing "Uses kappa control distances"
    (t/is (= [[:move [2.0 0.0]]
              [:cubic [2.0 0.5522847498307936] [1.1045694996615871 1.0] [0.0 1.0]]
              [:cubic [-1.1045694996615871 1.0] [-2.0 0.5522847498307936] [-2.0 0.0]]
              [:cubic [-2.0 -0.5522847498307936] [-1.1045694996615871 -1.0] [0.0 -1.0]]
              [:cubic [1.1045694996615871 -1.0] [2.0 -0.5522847498307936] [2.0 0.0]]]
             (sut/ellipse->cubics {:centre [0 0] :rx 2 :ry 1}))))

  (t/testing "Non-positive or invalid radii throw"
    (t/is (illegal-arg-thrown? #(sut/ellipse->cubics {:centre [0 0] :rx 0 :ry 1})))
    (t/is (illegal-arg-thrown? #(sut/ellipse->cubics {:centre [0 0] :rx 1 :ry -1})))
    (t/is (illegal-arg-thrown? #(sut/ellipse->cubics {:centre [0 0] :rx 1})))
    (t/is (illegal-arg-thrown? #(sut/ellipse->cubics {:centre [0] :rx 1 :ry 1})))))
