(ns nicheware.platform.utilities.common.graphics-test
  (:require [nicheware.platform.utilities.common.graphics :as sut]
            [nicheware.platform.utilities.common.math :as math]
            [nicheware.platform.utilities.common.graphics.line :as line]
            [clojure.set :as set]
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


;; ======================== Affine transform functions =================================

(defn- matrix-approx=
  ([m n] (matrix-approx= m n 1e-9))
  ([m n tolerance] (and (= 6 (count m) (count n)) (points-approx= m n tolerance))))

(def test-points [[0 0] [1 0] [0 1] [2.5 -3] [-7 4.25] [100 -50]])

(def test-matrices
  [[1 0 0 1 0 0]
   [2 0 0 3 10 20]
   [0.5 1.5 -2 0.25 -3 7]
   (sut/rotate-matrix 30 [4 -2])
   [1 0 0 -1 0 100]])

(t/deftest test-identity-matrix
  (t/testing "Identity is doubles and equal to an empty composition"
    (t/is (= [1.0 0.0 0.0 1.0 0.0 0.0] sut/identity-matrix))
    (t/is (every? float? sut/identity-matrix))
    (t/is (= sut/identity-matrix (sut/compose-matrix))))
  (t/testing "Identity leaves points unchanged"
    (doseq [p test-points]
      (t/is (= (mapv double p) (sut/apply-matrix sut/identity-matrix p))))))

(t/deftest test-apply-matrix
  (t/testing "x' = a·x + c·y + e, y' = b·x + d·y + f"
    (t/is (= [12.0 23.0] (sut/apply-matrix [2 0 0 3 10 20] [1 1])))
    (t/is (= [(+ (* 1 3) (* 3 4) 5.0) (+ (* 2 3) (* 4 4) 6.0)]
             (sut/apply-matrix [1 2 3 4 5 6] [3 4]))))
  (t/testing "Any numbers in, doubles out"
    (t/is (every? float? (sut/apply-matrix [1 0 0 1 (/ 1 2) 0] [1 2])))
    (t/is (= [1.5 2.0] (sut/apply-matrix [1 0 0 1 (/ 1 2) 0] [1 2]))))
  (t/testing "Invalid arguments throw"
    (t/is (illegal-arg-thrown? #(sut/apply-matrix [1 0 0 1 0] [1 2])))
    (t/is (illegal-arg-thrown? #(sut/apply-matrix nil [1 2])))
    (t/is (illegal-arg-thrown? #(sut/apply-matrix [1 0 0 1 0 "0"] [1 2])))
    (t/is (illegal-arg-thrown? #(sut/apply-matrix sut/identity-matrix [1])))
    (t/is (illegal-arg-thrown? #(sut/apply-matrix sut/identity-matrix nil)))))

(t/deftest test-compose-matrix
  (t/testing "Arities"
    (t/is (= sut/identity-matrix (sut/compose-matrix)))
    (t/is (= [2.0 0.0 0.0 3.0 10.0 20.0] (sut/compose-matrix [2 0 0 3 10 20]))))
  (t/testing "Applies the rightmost matrix first (SVG transform-list order)"
    (t/is (= [2.0 0.0 0.0 2.0 10.0 0.0]
             (sut/compose-matrix (sut/translate-matrix [10 0]) (sut/scale-matrix 2))))
    (t/is (= [2.0 0.0 0.0 2.0 20.0 0.0]
             (sut/compose-matrix (sut/scale-matrix 2) (sut/translate-matrix [10 0])))))
  (t/testing "apply (compose m1 m2) p = apply m1 (apply m2 p)"
    (doseq [m1 test-matrices m2 test-matrices p test-points]
      (t/is (points-approx= (sut/apply-matrix m1 (sut/apply-matrix m2 p))
                            (sut/apply-matrix (sut/compose-matrix m1 m2) p)
                            1e-9))))
  (t/testing "Three or more compose right to left, and are associative"
    (doseq [m1 test-matrices m2 test-matrices m3 (take 3 test-matrices) p (take 3 test-points)]
      (t/is (points-approx= (sut/apply-matrix m1 (sut/apply-matrix m2 (sut/apply-matrix m3 p)))
                            (sut/apply-matrix (sut/compose-matrix m1 m2 m3) p)
                            1e-9))
      (t/is (matrix-approx= (sut/compose-matrix (sut/compose-matrix m1 m2) m3)
                            (sut/compose-matrix m1 (sut/compose-matrix m2 m3))))))
  (t/testing "Invalid matrices throw"
    (t/is (illegal-arg-thrown? #(sut/compose-matrix [1 2 3])))
    (t/is (illegal-arg-thrown? #(sut/compose-matrix sut/identity-matrix nil)))
    (t/is (illegal-arg-thrown? #(sut/compose-matrix sut/identity-matrix sut/identity-matrix [1 0 0 1 0 :x])))))

(t/deftest test-primitive-matrices
  (t/testing "translate and scale"
    (t/is (= [1.0 0.0 0.0 1.0 3.0 -4.0] (sut/translate-matrix [3 -4])))
    (t/is (= [2.0 0.0 0.0 2.0 0.0 0.0] (sut/scale-matrix 2)))
    (t/is (= [2.0 0.0 0.0 0.5 0.0 0.0] (sut/scale-matrix [2 (/ 1 2)]))))
  (t/testing "rotate is clockwise on screen in y-down coordinates: +x turns towards +y"
    (t/is (points-approx= [0.0 1.0] (sut/apply-matrix (sut/rotate-matrix 90) [1 0])))
    (t/is (points-approx= [-1.0 0.0] (sut/apply-matrix (sut/rotate-matrix 90) [0 1])))
    (t/is (matrix-approx= [0.0 1.0 -1.0 0.0 0.0 0.0] (sut/rotate-matrix 90))))
  (t/testing "rotate about a point keeps that point fixed"
    (t/is (points-approx= [4.0 -2.0] (sut/apply-matrix (sut/rotate-matrix 37 [4 -2]) [4 -2])))
    (t/is (points-approx= [1.0 1.0] (sut/apply-matrix (sut/rotate-matrix 90 [1 0]) [2 0]))))
  (t/testing "Invalid arguments throw"
    (t/is (illegal-arg-thrown? #(sut/translate-matrix 3)))
    (t/is (illegal-arg-thrown? #(sut/scale-matrix "2")))
    (t/is (illegal-arg-thrown? #(sut/scale-matrix [1 2 3])))
    (t/is (illegal-arg-thrown? #(sut/rotate-matrix nil)))
    (t/is (illegal-arg-thrown? #(sut/rotate-matrix 90 [1])))))

(t/deftest test-transform->matrix
  (t/testing "Legacy {:x :y :scale} form"
    (t/is (= [2.0 0.0 0.0 2.0 10.0 20.0] (sut/transform->matrix {:x 10 :y 20 :scale 2})))
    (t/is (= [1.0 0.0 0.0 1.0 5.0 0.0] (sut/transform->matrix {:x 5})))
    (t/is (= [3.0 0.0 0.0 3.0 0.0 0.0] (sut/transform->matrix {:scale 3})))
    (t/is (= sut/identity-matrix (sut/transform->matrix {})))
    (t/is (= [5.0 0.0 0.0 5.0 25.0 0.0]
             (sut/transform->matrix (sut/position-to-dimensions {:width 100 :height 100} {:width 10 :height 20})))
          "Extra keys such as :width/:height are ignored"))

  (t/testing "reverse-transform undoes apply-matrix of the legacy transform"
    (doseq [transform [{:x 175.501 :y 0 :scale 2.3333} {:x -3 :y 8 :scale 0.5}]
            [px py] test-points]
      (let [[tx ty] (sut/apply-matrix (sut/transform->matrix transform) [px py])
            {:keys [x y]} (sut/reverse-transform tx ty transform)]
        (t/is (points-approx= [px py] [x y] 1e-9)))))

  (t/testing "Component form defaults"
    (t/is (= sut/identity-matrix (sut/transform->matrix {:translate [0 0]})))
    (t/is (= [1.0 0.0 0.0 1.0 10.0 -2.0] (sut/transform->matrix {:translate [10 -2]})))
    (t/is (= [2.0 0.0 0.0 3.0 0.0 0.0] (sut/transform->matrix {:scale [2 3]})))
    (t/is (matrix-approx= (sut/rotate-matrix 30) (sut/transform->matrix {:rotate {:degrees 30}}))))

  (t/testing "Hand computed: rotate 90 about [1 0], scale 2, translate [10 0] applied to [2 0]"
    ;; [2 0] -rotate 90 about [1 0]-> [1 1] -scale 2-> [2 2] -translate-> [12 2]
    (t/is (points-approx= [12.0 2.0]
                          (sut/apply-matrix (sut/transform->matrix {:translate [10 0]
                                                                     :scale 2
                                                                     :rotate {:degrees 90 :about [1 0]}})
                                            [2 0]))))

  (t/testing "Equivalent to SVG \"translate(t) scale(s) rotate(r about)\""
    (doseq [t [[0 0] [10 -4] [-2.5 7]]
            s [1 2 [0.5 3]]
            [degrees about] [[0 [0 0]] [90 [1 0]] [-33 [5 5]] [210 [-1 2]]]]
      (let [m (sut/transform->matrix {:translate t :scale s :rotate {:degrees degrees :about about}})
            svg (sut/compose-matrix (sut/translate-matrix t) (sut/scale-matrix s) (sut/rotate-matrix degrees about))]
        (doseq [p test-points]
          (t/is (points-approx= (sut/apply-matrix svg p) (sut/apply-matrix m p) 1e-9))))))

  (t/testing "Invalid transforms throw"
    (t/is (illegal-arg-thrown? #(sut/transform->matrix nil)))
    (t/is (illegal-arg-thrown? #(sut/transform->matrix [1 0 0 1 0 0])))
    (t/is (illegal-arg-thrown? #(sut/transform->matrix {:x 1 :translate [1 2]})))
    (t/is (illegal-arg-thrown? #(sut/transform->matrix {:y 1 :rotate {:degrees 3}})))
    (t/is (illegal-arg-thrown? #(sut/transform->matrix {:x "1"})))
    (t/is (illegal-arg-thrown? #(sut/transform->matrix {:x 1 :scale nil})))
    (t/is (illegal-arg-thrown? #(sut/transform->matrix {:x 1 :scale [1 2]})))
    (t/is (illegal-arg-thrown? #(sut/transform->matrix {:translate 3})))
    (t/is (illegal-arg-thrown? #(sut/transform->matrix {:scale :big})))
    (t/is (illegal-arg-thrown? #(sut/transform->matrix {:rotate 90})))
    (t/is (illegal-arg-thrown? #(sut/transform->matrix {:rotate {:about [1 2]}})))
    (t/is (illegal-arg-thrown? #(sut/transform->matrix {:rotate {:degrees 90 :about 1}})))))

(t/deftest test-invert-matrix
  (t/testing "Simple inverse"
    (t/is (= [0.5 0.0 0.0 0.25 -5.0 -5.0] (sut/invert-matrix [2 0 0 4 10 20]))))
  (t/testing "compose(m, inv m) and compose(inv m, m) are the identity"
    (doseq [m test-matrices]
      (t/is (matrix-approx= sut/identity-matrix (sut/compose-matrix m (sut/invert-matrix m))))
      (t/is (matrix-approx= sut/identity-matrix (sut/compose-matrix (sut/invert-matrix m) m)))))
  (t/testing "Undoes apply-matrix"
    (doseq [m test-matrices p test-points]
      (t/is (points-approx= p (sut/apply-matrix (sut/invert-matrix m) (sut/apply-matrix m p)) 1e-9))))
  (t/testing "Flip conjugation: rotation in a y-up frame becomes the opposite rotation"
    (let [flip [1 0 0 -1 0 0]
          conjugated (sut/compose-matrix flip (sut/rotate-matrix 90) (sut/invert-matrix flip))]
      (t/is (matrix-approx= (sut/rotate-matrix -90) conjugated))))
  (t/testing "Singular or invalid matrices throw"
    (t/is (illegal-arg-thrown? #(sut/invert-matrix [0 0 0 0 1 1])))
    (t/is (illegal-arg-thrown? #(sut/invert-matrix [1 2 2 4 0 0])))
    (t/is (illegal-arg-thrown? #(sut/invert-matrix [1e-7 0 0 1e-7 0 0])))
    (t/is (illegal-arg-thrown? #(sut/invert-matrix [1 0 0 1])))))


;; ======================== Bounding box functions =================================

(defn- box-approx=
  ([b1 b2] (box-approx= b1 b2 1e-9))
  ([b1 b2 tolerance]
   (points-approx= (map b1 [:x :y :width :height]) (map b2 [:x :y :width :height]) tolerance)))

(defn- box-contains?
  [{:keys [x y width height]} [px py] tolerance]
  (and (<= (- x tolerance) px (+ x width tolerance))
       (<= (- y tolerance) py (+ y height tolerance))))

(defn- sample-path
  "Samples every cubic of a path (with dense sampling) plus all :move/:line points."
  [segments samples]
  (:points
   (reduce (fn [{:keys [current] :as state} [op & args]]
             (case op
               (:move :line) (-> state (assoc :current (first args)) (update :points conj (first args)))
               :cubic (let [[c1 c2 p] args
                            curve (line/bezier-cubic-equation current c1 c2 p)]
                        (-> state
                            (assoc :current p)
                            (update :points into (map #(vec (curve (/ % (dec samples)))) (range samples)))))
               :close state))
           {:current nil :points []}
           segments)))

(defn- sampled-box
  [segments samples]
  (sut/bounding-box (sample-path segments samples)))

(def test-cubic-paths
  {:arch [[:move [0 0]] [:cubic [0 10] [10 10] [10 0]]]
   :s-curve [[:move [0 0]] [:cubic [10 -20] [-10 30] [5 5]]]
   :loop [[:move [0 0]] [:cubic [30 20] [-20 20] [10 0]]]
   :far-controls [[:move [0 0]] [:cubic [100 -80] [-90 120] [3 2]]]
   :straight-cubic [[:move [0 0]] [:cubic [1 1] [2 2] [3 3]]]
   :multi [[:move [5 5]] [:line [20 5]] [:cubic [25 -10] [30 25] [15 15]] [:close]
           [:move [-5 -5]] [:cubic [-15 -5] [-15 5] [-5 5]]]})

(t/deftest test-bounding-box
  (t/testing "Points"
    (t/is (= {:x -1.0 :y 2.0 :width 5.0 :height 3.0} (sut/bounding-box [[1 5] [4 2] [-1 3]])))
    (t/is (= {:x 3.0 :y 4.0 :width 0.0 :height 0.0} (sut/bounding-box [[3 4]])))
    (t/is (= {:x 0.5 :y 0.0 :width 0.5 :height 1.0} (sut/bounding-box [[(/ 1 2) 0] [1 1]]))))
  (t/testing "Empty is nil"
    (t/is (nil? (sut/bounding-box nil)))
    (t/is (nil? (sut/bounding-box []))))
  (t/testing "Invalid points throw"
    (t/is (illegal-arg-thrown? #(sut/bounding-box [[1 2] [3]])))
    (t/is (illegal-arg-thrown? #(sut/bounding-box [[1 2] nil])))))

(t/deftest test-segments-bounding-box
  (t/testing "Arch: tight box, not the control point hull"
    (t/is (= {:x 0.0 :y 0.0 :width 10.0 :height 7.5}
             (sut/segments-bounding-box (:arch test-cubic-paths)))))

  (doseq [[path-name segments] test-cubic-paths]
    (let [box (sut/segments-bounding-box segments)
          samples (sample-path segments 4001)
          sampled (sut/bounding-box samples)]
      (t/testing (str path-name " contains every dense sample")
        (t/is (every? #(box-contains? box % 1e-9) samples)))
      (t/testing (str path-name " is tight: within the sampling error of the sampled extrema")
        (t/is (box-approx= sampled box 1e-3)))))

  (t/testing "Tight within 1e-9 at the solved extrema parameters"
    (doseq [[path-name segments] (dissoc test-cubic-paths :multi)]
      (let [[[_ start] [_ c1 c2 end]] segments
            curve (line/bezier-cubic-equation start c1 c2 end)
            ts (concat [0 1]
                       (sut/cubic-extrema-parameters (start 0) (c1 0) (c2 0) (end 0))
                       (sut/cubic-extrema-parameters (start 1) (c1 1) (c2 1) (end 1)))
            exact (sut/bounding-box (map #(vec (curve %)) ts))]
        (t/is (box-approx= exact (sut/segments-bounding-box segments) 1e-9) (str path-name)))))

  (t/testing "Control points far outside the curve: hull would be much too big"
    (let [segments (:far-controls test-cubic-paths)
          [[_ start] [_ c1 c2 end]] segments
          hull (sut/bounding-box [start c1 c2 end])
          box (sut/segments-bounding-box segments)]
      (t/is (< (:width box) (* 0.6 (:width hull))))
      (t/is (< (:height box) (* 0.6 (:height hull))))))

  (t/testing "Unrotated ellipse box ≈ [cx−rx, cy−ry, 2rx, 2ry]"
    (doseq [[cx cy rx ry] [[0 0 2 1] [100 50 40 15] [-20 30 25 25]]]
      (t/is (box-approx= {:x (- cx rx) :y (- cy ry) :width (* 2 rx) :height (* 2 ry)}
                         (sut/segments-bounding-box (sut/ellipse->cubics {:centre [cx cy] :rx rx :ry ry}))
                         (* 0.0003 (max rx ry))))))

  (t/testing "Rotated ellipse matches its analytic box within the approximation tolerance"
    (let [rx 60.0 ry 20.0 phi (to-radians 30)
          half-w (Math/sqrt (+ (* rx rx (Math/pow (Math/cos phi) 2)) (* ry ry (Math/pow (Math/sin phi) 2))))
          half-h (Math/sqrt (+ (* rx rx (Math/pow (Math/sin phi) 2)) (* ry ry (Math/pow (Math/cos phi) 2))))]
      (t/is (box-approx= {:x (- 10 half-w) :y (- -5 half-h) :width (* 2 half-w) :height (* 2 half-h)}
                         (sut/segments-bounding-box (sut/ellipse->cubics {:centre [10 -5] :rx rx :ry ry :x-rotation 30}))
                         0.02))))

  (t/testing "Lines, moves and close"
    (t/is (= {:x 0.0 :y -1.0 :width 4.0 :height 3.0}
             (sut/segments-bounding-box [[:move [0 0]] [:line [4 2]] [:line [1 -1]] [:close]])))
    (t/is (= {:x 1.0 :y 1.0 :width 0.0 :height 0.0} (sut/segments-bounding-box [[:move [1 1]]]))))

  (t/testing "A cubic after :close starts from the subpath start"
    (t/is (= {:x 0.0 :y 0.0 :width 10.0 :height 10.0}
             (sut/segments-bounding-box [[:move [0 0]] [:line [10 0]] [:close] [:cubic [0 0] [0 0] [0 10]]]))))

  (t/testing "Empty is nil"
    (t/is (nil? (sut/segments-bounding-box nil)))
    (t/is (nil? (sut/segments-bounding-box [])))
    (t/is (nil? (sut/segments-bounding-box [[:close]]))))

  (t/testing "Invalid paths throw"
    (t/is (illegal-arg-thrown? #(sut/segments-bounding-box [[:cubic [0 1] [1 1] [1 0]]])))
    (t/is (illegal-arg-thrown? #(sut/segments-bounding-box [[:close] [:cubic [0 1] [1 1] [1 0]]])))
    (t/is (illegal-arg-thrown? #(sut/segments-bounding-box [[:move [0 0]] [:arc [1 1]]])))
    (t/is (illegal-arg-thrown? #(sut/segments-bounding-box [[:move [0]]])))
    (t/is (illegal-arg-thrown? #(sut/segments-bounding-box [[:move [0 0]] [:cubic [0 1] [1 1]]])))
    (t/is (illegal-arg-thrown? #(sut/segments-bounding-box [:move [0 0]])))))

(t/deftest test-quadratic-roots
  (t/testing "Degenerate cases"
    (t/is (= [] (sut/quadratic-roots 0.0 0.0 0.0)))
    (t/is (= [] (sut/quadratic-roots 0.0 0.0 5.0)))
    (t/is (= [0.5] (sut/quadratic-roots 0.0 2.0 -1.0)))
    (t/is (= [] (sut/quadratic-roots 1.0 0.0 1.0))))
  (t/testing "Two roots"
    (t/is (= #{2.0 3.0} (set (sut/quadratic-roots 1.0 -5.0 6.0))))))

(t/deftest test-bounding-box-union
  (t/testing "Union"
    (t/is (= {:x 0.0 :y -1.0 :width 6.0 :height 3.0}
             (sut/bounding-box-union [{:x 0 :y 0 :width 2 :height 2} nil {:x 5 :y -1 :width 1 :height 1}])))
    (t/is (= {:x 1.0 :y 2.0 :width 3.0 :height 4.0}
             (sut/bounding-box-union [{:x 1 :y 2 :width 3 :height 4}]))))
  (t/testing "Nothing is nil"
    (t/is (nil? (sut/bounding-box-union nil)))
    (t/is (nil? (sut/bounding-box-union [])))
    (t/is (nil? (sut/bounding-box-union [nil nil]))))
  (t/testing "Malformed boxes throw"
    (t/is (illegal-arg-thrown? #(sut/bounding-box-union [{:x 0 :y 0 :width 1}])))
    (t/is (illegal-arg-thrown? #(sut/bounding-box-union [{:x 0 :y 0 :width -1 :height 1}])))
    (t/is (illegal-arg-thrown? #(sut/bounding-box-union [[0 0 1 1]])))))

(t/deftest test-transform-bounding-box
  (t/testing "Scale and translate"
    (t/is (= {:x 12.0 :y 2.0 :width 6.0 :height 8.0}
             (sut/transform-bounding-box (sut/transform->matrix {:x 10 :y 0 :scale 2})
                                         {:x 1 :y 1 :width 3 :height 4}))))
  (t/testing "Rotation gives the box of the rotated corners"
    (t/is (box-approx= {:x -4.0 :y 0.0 :width 4.0 :height 2.0}
                       (sut/transform-bounding-box (sut/rotate-matrix 90) {:x 0 :y 0 :width 2 :height 4})))
    (let [s (Math/sqrt 2)]
      (t/is (box-approx= {:x (- (/ s 2)) :y 0.0 :width s :height s}
                         (sut/transform-bounding-box (sut/rotate-matrix 45) {:x 0 :y 0 :width 1 :height 1})))))
  (t/testing "Contains the transformed contents' exact box (conservative)"
    (let [segments (sut/ellipse->cubics {:centre [0 0] :rx 3 :ry 1})
          m (sut/transform->matrix {:translate [5 5] :rotate {:degrees 30}})
          transformed (mapv (fn [[op & points]] (into [op] (map #(sut/apply-matrix m %)) points)) segments)
          exact (sut/segments-bounding-box transformed)
          conservative (sut/transform-bounding-box m (sut/segments-bounding-box segments))]
      (t/is (= conservative (sut/bounding-box-union [conservative exact])))))
  (t/testing "nil box is nil"
    (t/is (nil? (sut/transform-bounding-box sut/identity-matrix nil))))
  (t/testing "Invalid arguments throw"
    (t/is (illegal-arg-thrown? #(sut/transform-bounding-box [1 0 0] {:x 0 :y 0 :width 1 :height 1})))
    (t/is (illegal-arg-thrown? #(sut/transform-bounding-box sut/identity-matrix {:x 0 :y 0})))))


;; ======================== Hatching functions =================================

(def hatch-directions
  "Expected unit-free line directions per line angle."
  {0 [1 0] 90 [0 -1] 45 [1 -1] 135 [1 1]})

(defn- on-rect-boundary?
  [{:keys [x y width height]} [px py]]
  (let [right (+ x width) bottom (+ y height)]
    (and (<= (- x 1e-9) px (+ right 1e-9))
         (<= (- y 1e-9) py (+ bottom 1e-9))
         (or (approx= px x) (approx= px right) (approx= py y) (approx= py bottom)))))

(defn- parallel?
  [[dx dy] [[x1 y1] [x2 y2]]]
  (approx= 0.0 (- (* dx (- y2 y1)) (* dy (- x2 x1))) 1e-9))

(defn- offset
  "Perpendicular offset of the line through p at angle (screen degrees): n·p, n = (sin θ, cos θ)."
  [angle [px py]]
  (let [theta (to-radians angle)]
    (+ (* (Math/sin theta) px) (* (Math/cos theta) py))))

(defn- brute-force-line-count
  "Number of k with k·pitch between the min and max offsets of the rect corners. An extreme offset is
   included only when two corners share it (a line along an edge); at a single corner it is degenerate."
  [{:keys [x y width height]} angle pitch]
  (let [offsets (map #(offset angle %) [[x y] [(+ x width) y] [x (+ y height)] [(+ x width) (+ y height)]])
        low (apply min offsets)
        high (apply max offsets)
        shared? (fn [extreme] (<= 2 (count (filter #(approx= extreme %) offsets))))
        low-bound (if (shared? low) (- low 1e-9) (+ low 1e-9))
        high-bound (if (shared? high) (+ high 1e-9) (- high 1e-9))]
    (count (filter #(<= low-bound (* % pitch) high-bound) (range -1000 1000)))))

(def hatch-rects
  [{:x 0 :y 0 :width 10 :height 5}
   {:x 3.3 :y -7.1 :width 12.7 :height 4.9}
   {:x -20 :y 10 :width 3 :height 17}])

(t/deftest test-hatch-rect-lines
  (doseq [rect hatch-rects
          [pattern angle] [[:horizontal 0] [:vertical 90] [:diagonal-45 45] [:diagonal-135 135] [30 30] [-100 -100]]
          pitch [0.7 2]]
    (let [{:keys [lines dots]} (sut/hatch-rect rect {:pattern pattern :pitch pitch})
          direction (get hatch-directions angle [(Math/cos (to-radians angle)) (- (Math/sin (to-radians angle)))])
          offsets (map #(offset angle (first %)) lines)]
      (t/testing (str rect " " pattern " pitch " pitch)
        (t/is (= [] dots))
        (t/is (seq lines))
        (t/is (every? #(every? (partial on-rect-boundary? rect) %) lines) "Endpoints on the rect boundary")
        (t/is (every? (partial parallel? direction) lines) "Lines have the pattern direction")
        (t/is (every? (fn [[p q]] (neg? (compare p q))) lines) "Endpoints ordered smaller x then y")
        (t/is (every? #(every? float? (flatten %)) lines) "Doubles")
        (t/is (every? #(approx= (Math/abs pitch) (Math/abs %) 1e-9) (map - (rest offsets) offsets))
              "Consecutive offsets are pitch apart")
        (t/is (or (apply <= offsets) (apply >= offsets)) "Sorted by offset")
        (t/is (every? #(approx= 0.0 (let [r (/ % pitch)] (- r (Math/round r))) 1e-9) offsets)
              "Anchored to the global origin")
        (t/is (= (brute-force-line-count rect angle pitch) (count lines)) "Brute force count")))))

(t/deftest test-hatch-rect-examples
  (t/testing "Horizontal worked example, including lines on the boundary"
    (t/is (= {:lines [[[0.0 0.0] [10.0 0.0]] [[0.0 2.0] [10.0 2.0]] [[0.0 4.0] [10.0 4.0]]] :dots []}
             (sut/hatch-rect {:x 0 :y 0 :width 10 :height 5} {:pattern :horizontal :pitch 2}))))
  (t/testing "45 rises left to right on screen; corner-touching line dropped"
    (let [{:keys [lines]} (sut/hatch-rect {:x 0 :y 0 :width 1 :height 1} {:pattern :diagonal-45 :pitch (Math/sqrt 0.5)})]
      (t/is (= 1 (count lines)))
      (t/is (points-approx= [0.0 1.0] (ffirst lines)))
      (t/is (points-approx= [1.0 0.0] (second (first lines))))))
  (t/testing "135 falls left to right on screen"
    (let [{:keys [lines]} (sut/hatch-rect {:x 0 :y 0 :width 1 :height 1} {:pattern :diagonal-135 :pitch 10})]
      (t/is (= [[[0.0 0.0] [1.0 1.0]]] lines))))
  (t/testing "Numeric 0 / 90 / 45 / 135 / 180 equal the named patterns exactly"
    (doseq [rect hatch-rects]
      (t/is (= (sut/hatch-rect rect {:pattern :horizontal :pitch 1.5}) (sut/hatch-rect rect {:pattern 0 :pitch 1.5})))
      (t/is (= (sut/hatch-rect rect {:pattern :horizontal :pitch 1.5}) (sut/hatch-rect rect {:pattern 180 :pitch 1.5})))
      (t/is (= (sut/hatch-rect rect {:pattern :horizontal :pitch 1.5}) (sut/hatch-rect rect {:pattern -180 :pitch 1.5})))
      (t/is (= (sut/hatch-rect rect {:pattern :vertical :pitch 1.5}) (sut/hatch-rect rect {:pattern 90 :pitch 1.5})))
      (t/is (= (sut/hatch-rect rect {:pattern :diagonal-45 :pitch 1.5}) (sut/hatch-rect rect {:pattern 45.0 :pitch 1.5})))
      (t/is (= (sut/hatch-rect rect {:pattern :diagonal-135 :pitch 1.5}) (sut/hatch-rect rect {:pattern -45 :pitch 1.5})))))
  (t/testing ":cross is the 45 lines followed by the 135 lines"
    (doseq [rect hatch-rects]
      (t/is (= (into (:lines (sut/hatch-rect rect {:pattern :diagonal-45 :pitch 1.3}))
                     (:lines (sut/hatch-rect rect {:pattern :diagonal-135 :pitch 1.3})))
               (:lines (sut/hatch-rect rect {:pattern :cross :pitch 1.3}))))))
  (t/testing "Rect smaller than pitch gives 0 or 1 lines"
    (t/is (= [] (:lines (sut/hatch-rect {:x 0.2 :y 0.2 :width 0.5 :height 0.5} {:pattern :horizontal :pitch 1}))))
    (t/is (= [[[0.2 1.0] [0.7 1.0]]]
             (:lines (sut/hatch-rect {:x 0.2 :y 0.8 :width 0.5 :height 0.5} {:pattern :horizontal :pitch 1}))))
    (t/is (<= (count (:lines (sut/hatch-rect {:x 0.2 :y 0.2 :width 0.5 :height 0.5} {:pattern :diagonal-45 :pitch 1}))) 1)))
  (t/testing "Any numbers are accepted"
    (t/is (= (sut/hatch-rect {:x 0.5 :y 0 :width 2.5 :height 2} {:pattern :vertical :pitch 0.5})
             (sut/hatch-rect {:x (/ 1 2) :y 0 :width (/ 5 2) :height 2} {:pattern :vertical :pitch (/ 1 2)})))))

(t/deftest test-hatch-rect-adjacent
  (t/testing "Adjacent rects' lines meet at the shared edge"
    (doseq [pattern [:diagonal-45 :diagonal-135 :horizontal 30 -70]]
      (let [left {:x 0 :y 0 :width 7.3 :height 5}
            right {:x 7.3 :y 0 :width 4.1 :height 5}
            edge-points (fn [rect side]
                          (->> (:lines (sut/hatch-rect rect {:pattern pattern :pitch 0.9}))
                               (mapcat identity)
                               (filter #(approx= 7.3 (first %)))
                               (map #(nth % 1))
                               sort))
            from-left (edge-points left :right)
            from-right (edge-points right :left)]
        (t/is (seq from-left) (str pattern))
        (t/is (= (count from-left) (count from-right)) (str pattern))
        (t/is (points-approx= from-left from-right 1e-9) (str pattern))))))

(t/deftest test-hatch-rect-dots
  (t/testing "Half-open grid, sorted by y then x"
    (t/is (= {:lines [] :dots [[0.0 0.0] [2.0 0.0]]}
             (sut/hatch-rect {:x 0 :y 0 :width 4 :height 2} {:pattern :dots :pitch 2})))
    (t/is (= [[2.0 -2.0] [4.0 -2.0] [2.0 0.0] [4.0 0.0]]
             (:dots (sut/hatch-rect {:x 1 :y -3 :width 3.5 :height 4} {:pattern :dots :pitch 2})))))
  (t/testing "Adjacent rects never duplicate a dot and together cover the union"
    (let [dots-of #(set (:dots (sut/hatch-rect % {:pattern :dots :pitch 1})))
          a (dots-of {:x 0 :y 0 :width 3 :height 2})
          b (dots-of {:x 3 :y 0 :width 2 :height 2})
          c (dots-of {:x 0 :y 2 :width 5 :height 1})]
      (t/is (empty? (set/intersection a b)))
      (t/is (empty? (set/intersection a c)))
      (t/is (= (dots-of {:x 0 :y 0 :width 5 :height 3}) (set/union a b c)))))
  (t/testing "Rect smaller than pitch may have no dots"
    (t/is (= [] (:dots (sut/hatch-rect {:x 0.1 :y 0.1 :width 0.5 :height 0.5} {:pattern :dots :pitch 1}))))))

(t/deftest test-hatch-rect-errors
  (let [rect {:x 0 :y 0 :width 1 :height 1}]
    (t/is (illegal-arg-thrown? #(sut/hatch-rect rect {:pattern :zigzag :pitch 1})))
    (t/is (illegal-arg-thrown? #(sut/hatch-rect rect {:pattern nil :pitch 1})))
    (t/is (illegal-arg-thrown? #(sut/hatch-rect rect {:pattern :horizontal :pitch 0})))
    (t/is (illegal-arg-thrown? #(sut/hatch-rect rect {:pattern :horizontal :pitch -1})))
    (t/is (illegal-arg-thrown? #(sut/hatch-rect rect {:pattern :horizontal})))
    (t/is (illegal-arg-thrown? #(sut/hatch-rect {:x 0 :y 0 :width 0 :height 1} {:pattern :horizontal :pitch 1})))
    (t/is (illegal-arg-thrown? #(sut/hatch-rect {:x 0 :y 0 :width 1 :height -1} {:pattern :dots :pitch 1})))
    (t/is (illegal-arg-thrown? #(sut/hatch-rect {:x 0 :y 0 :width 1} {:pattern :dots :pitch 1})))
    (t/is (illegal-arg-thrown? #(sut/hatch-rect nil {:pattern :dots :pitch 1})))))
