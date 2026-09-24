(ns nicheware.platform.utilities.common.graphics.color
"
Functions for dealing with colors and conversion between different color representations.

There are groups of functions and variables within the color namespace that deal with:

|Function group|Functions|
|---|---|
|constants for argb packed int colors |[[blue-color]], [[black-color]], [[light-grey-color]], [[dark-red-color]], [[transparent-blue-color]], [[dark-blue-color]], [[white-color]], [[dark-green-color]], [[green-color]], [[yellow-color]]|
|constants for css string hex colors| [[bright-red-color]], [[css-yellow-color]], [[css-blue-color]], [[css-pink-color]], [[css-white-color]]|
|rgb utility functions| [[normalise-rgb]], [[int-rgb]], [[ratio-rgba-to-int-rgba]], [[nudge-color]], [[unique-color]]|
|rgb format conversion| [[color-as-rgba]], [[color-as-map]], [[rgba-as-css-hex]], [[color-as-css-hex]],  [[react-color-as-rgba]]|
|rgba, hsla color model conversion| [[rgba-to-hsla]], [[hsla-to-rgba]]|
|color model keyword functions| [[model-from-to]], [[model-ranges]]|
|color gradient functions| [[make-color-increment-fn]], [[color-difference-seq]]|
|luminance and contrast| [[relative-luminance]], [[contrast-ratio]]|

The different color representations that are used by functions include:

|Representation|Description|
|--|--|
|RGBA| an RGB color with an alpha channel, represented as a vector with 4 values ```[R G B A]```. Each value is an int from 0 to 255|
|RGB or packed RGB| an RGB color with an alpha channel, packed as a Clojure integer, with each byte representing A, R, G, B. The unpacked values are from 0 to 255|
|RGB CSS| String hex representation of an RGB color with no alpha channel, as used in CSS color specifications.|
|HSLA| Hue, Saturation, Lightness, Alpha as a clojure vector ```[H S L A]```.  **Hue** is a degree on the color wheel, 0 to 360.  **Saturation** is a percentage (100 full color),  **Lightness** is percentage of reflecting color (0 is dark, 100 is light),  **Alpha** is 0 to 255.|

Luminance and contrast follow the WCAG 2.x definitions, e.g. to order colors dark to light or to check
a text/background pair against a contrast floor:

```clojure
(relative-luminance [255 255 255 255])                 => 1.0
(sort-by relative-luminance [[255 255 255] [0 0 0] [128 128 128]])
                                                       => ([0 0 0] [128 128 128] [255 255 255])
(contrast-ratio [0 0 0 255] 0xFFFFFFFF)                => 21.0
(>= (contrast-ratio [119 119 119] [255 255 255]) 4.5)  => false
```

"
  (:require [thi.ng.strf.core :as f]
            [nicheware.platform.utilities.common.math :as math]
            #?(:clj  [clojure.core.match :refer [match]]
               :cljs [cljs.core.match :refer-macros [match]])
            [nicheware.platform.utilities.common.core :as common]))

;; ================================ Constants ===================================

(def hsl-precision 2)

;; ================================= Colors ======================================

;; Named color values
(def blue-color 0xFF84D4E4)
(def black-color 0xFF101010)
(def light-grey-color 0xFF444444)
(def dark-red-color 0xFFAA1111)
(def transparent-blue-color 0x4484D4E4)
(def dark-blue-color 0xFF84D4FF)
(def white-color 0xFFFFFFFF)
(def dark-green-color 0xFF1BC727)
(def green-color 0xFF11AA11)
(def yellow-color 0xFFAAAA11)

;; Css colors
(def bright-red-color "#f44336" )
(def css-yellow-color "#FFEB3B" )
(def css-blue-color "#84D4E4")
(def css-pink-color "#E91E63")
(def css-white-color "#FFFFFF")

;; ================================= Colors ======================================

(defn normalise-rgb
  "Converts a 0 to 255 RGB value to 0 to 1.0

  - value: int RGB value from 0 to 255
  - returns: float RGB value from 0 to 1.0
"
  [value]
  (float (/ value 255)))


(defn int-rgb
  "Converts a normalized value from 0 to 1 to an RGB int from 0 to 255

  - value: float RGB value from 0 to 1.0
  - returns: int RGB value from 0 to 255
"
  [normalised-value]
  (int (* 255 normalised-value)))

(defn ratio-rgba-to-int-rgba
  "Useful when RGB int values have been computed. Will convert any ratios to floats, round and then
  express as int

  - rgba-ratios: Collection of RGB values from 0 to 255. May be expressed as Clojure ratios due to calculations.
  - returns: vector of RGB values from 0 to 255 all as ints (may be rounded from original ratio value)
"
  [rgba-ratios]
  (into [] (map #(int (Math/round (float %))) rgba-ratios)))

;; Not using - so leave until we decide we do not need.
(comment
  (defn value-as-rgba
    "Converts a color value which is either an Int or a vector of ```[R G B]``` ints into
   a thi.ng color RGBA for computation.

  When this return results is de-referenced, it will be a vector of ```[R G B A]```
  all in the range 0 to 1"
    [color-value]
    (if (vector? color-value)
    ;;; We have a vector of value from 0 to 255, convert to 0 to 1
      (col/rgba (normalise-rgb (color-value 0))
                (normalise-rgb (color-value 1))
                (normalise-rgb (color-value 2))
                1)

      ;; We have an int of form <alpha><R><G><B>
      (-> color-value
          long
          col/int32
          col/as-rgba))))

;; ================================== RGB Color conversions (form not models) ==================

(defn color-as-rgba
  "Converts color values to vector of values from 0 to 255 for ```[R G B A]```.
   Accepts a packed int or existing vector of ```[R G B]``` or ```[R G B A]```.

   - color-value: Either a vector of ```[R G B]``` or ```[R G B A]```, or a packed int of form ```<alpha><R><G><B>```
   - returns: vector of form ```[R G B A]```. If no alpha provided on input, 255 (opaque) is used.
"
  [color-value]
  (if (not color-value)
    nil
    (if (coll? color-value)
      ;; We have a vector already, ensure we include an alpha for consistent handling
      (if (= 4 (count color-value))
        color-value
        (into [] (concat color-value [255])))

      ;; We have an int of form <alpha><R><G><B>
      [(bit-and (bit-shift-right color-value 16) 0xff)
       (bit-and (bit-shift-right color-value 8) 0xff)
       (bit-and color-value 0xff)
       (bit-and (unsigned-bit-shift-right color-value 24) 0xff)])))


(defn color-as-map
  "Converts color values to map of values from 0 to 255 for RGB and 0 to 1 for Alpha to
   ```{:r <red> :g <green> :b <blue> :a <alpha> }```

   Accepts a packed int or existing vector of ```[R G B]``` or ```[R G B A]```. Assumes alpha 255 on input.

   Output suitable for use with react-color pickers.

   - color-value: RGB color value as either ```[R G B]``` or ```[R G B A]```, or a packed int of form ```<alpha><R><G><B>```.
   - returns: color map of the form ```{:r <red> :g <green> :b <blue> :a <alpha> }```

"
  [color-value]
  (let [[red green blue alpha] (color-as-rgba color-value)]
    {:r red
     :g green
     :b blue
     :a (normalise-rgb alpha)}))

(defn rgba-as-css-hex
  "Transform a color vector of ```[R G B A]``` into a CSS hex string such as ```#FFFFFF```, ignoring
   the alpha.

  - color-value: vector of 0 to 255 values. ```[R G B A]```
  - returns: CSS hex RGB string ```#FFFFFF```
"
  [[red green blue alpha :as color-value]]
  (let [css-value (f/format ["#" (f/hex 2) (f/hex 2) (f/hex 2)] red green blue)]
;;    (println "color-vector-as-css-hex(): red: " red " green: " green " blue: " blue " css: " css-value)
    css-value))

(defn color-as-css-hex
  "Converts a color array of ```[R G B A]``` all from 0 to 255 or a packed int ARGB,
   into a CSS hex, which will ignore the alpha.

  - color-value: RGB color value as either ```[R G B]``` or ```[R G B A]```, or a packed int of form ```<alpha><R><G><B>```.
  - returns: CSS hex RGB string ```#FFFFFF```
"
  [color-value]
;;  (println "color-as-css-hex(): color-value: " color-value)
  (-> color-value
      color-as-rgba
      rgba-as-css-hex))

(defn react-color-as-rgba
  "Converts a react-col color value map. This has a number of forms within, but one is
  ```{ 'rgb' {'r' 255 'g' 255 'b' 255 'a' 1}}```. We convert this to ```[R G B A]```, all values 0 to 255

  - react-color: color as ```{ 'rgb' {'r' <red:0-255> 'g' <green:0-255> 'b' <blue:0-255> 'a' <alpha:0-1>}}```
  - returns: color as RGB vector ```[R G B A]```, all 0 to 255.

"
  [{rgb "rgb" :as react-color}]
  (let [{red "r" green "g" blue "b" alpha "a"} rgb]
    [red green blue (int-rgb alpha)]))

(defn nudge-color
  "Modifies the color in a small way to make a slightly different color.
   Currently does it by moving blue up or down by 1.

   - color: vector of RGBA color to be nudged. ```[R G B A]```
   - returns: slightly modified color in blue component. Either increment or decrement.

"
  [[red green blue alpha :as color]]
  (let [new-blue (if (< blue 125) (inc blue) (dec blue))]
    [red green new-blue alpha]))

(defn unique-color
  "Keep nudging the given color until it is unique given the existing set of unique colors.

   - col: vector of RGBA color to be nudged. ```[R G B A]```
   - unique-set: Set of RGBA colors, that we do not want to overlap with.
   - returns: slightly modified color in blue component. Either increment or decrement. Will be different from all other colors in unique-set.
"
  [col unique-set]
  (loop [current-col col
         test-set unique-set]
    (if (not (some #{current-col} test-set))
      current-col
      (recur (nudge-color current-col) test-set))))

;; ================================= RGBA to HSLA color model conversion ===================================

;; See http://www.rapidtables.com/convert/color/rgb-to-hsl.htm for formula and online calculator

(defn- calc-saturation
  "Computes saturation given working values from RGB -> HSL computation"
  [delta lightness]
  (if delta
    (math/roundn hsl-precision (/ delta
                      (- 1 (Math/abs (- (* 2 lightness)
                                        1)))))
    0))

(defn- calc-hue
  "Computes hue given working values from RGB -> HSL computation"
  [delta c-max [red green blue]]
  (let [delta-grad-fn  (fn [c1 c2 delta] (/ (- c1 c2) delta))
        raw-hue        (cond
                         (= delta 0)      0
                         (== c-max red)   (mod (delta-grad-fn green blue delta) 6)
                         (== c-max green) (+ 2 (delta-grad-fn blue red delta))
                         (== c-max blue)  (+ 4 (delta-grad-fn red green delta)))
        degrees        (int (Math/round (* 60 raw-hue)))
        normal-degrees (if (< degrees 0) (+ degrees 360) degrees)]
    normal-degrees))

(defn rgba-to-hsla
  "Converts an RGBA vector ```[R G B A]``` all 0 to 255, to HSLA vector ```[H S L A]```
   where H 0 to 360, and S and L are percentages and A remains unchanged (0 to 255)

   - rgba: vector of RGBA color. ```[R G B A]```
   - returns: HSLA version of rgba input. ```[H S L A]```
"
  [[_ _ _ alpha :as rgba]]
  (let [[red green blue :as normalised-rgb] (map normalise-rgb (take 3 rgba))
        c-max (apply max normalised-rgb)
        c-min (apply min normalised-rgb)
        delta  (- c-max c-min)
        lightness (math/roundn hsl-precision (* (+ c-max c-min) 0.5))
        saturation (calc-saturation delta lightness)
        hue (calc-hue delta c-max normalised-rgb)]
    [hue saturation lightness alpha ]))

;;  ========================================== HSLA to RGBA conversion =======================================

;; See formula at http://www.rapidtables.com/convert/color/hsl-to-rgb.htm

(defn- calc-c
  "Compures C in the HSL to RGB conversion formula"
  [saturation lightness]
  (* saturation
     (- 1
        (Math/abs (- (* 2 lightness) 1)))))

(defn- calc-x
  "Computes X in the HSL to RGB converson formula"
  [c hue]
  (* c (- 1 (Math/abs (float (- (mod (/ hue 60) 2)
                                 1))))))

(defn- calc-initial-rgb
  "Calculate initial raw RGB values for HSL to RGB calculation"
  [c x hue]
  (condp > hue
    60  [c x 0]
    120 [x c 0]
    180 [0 c x]
    240 [0 x c]
    300 [x 0 c]
    360 [c 0 x]
    ))

(defn- calc-rgb
  "Converts an initial rgb value in a HSL conversion to final RGB value 0 to 255"
  [m initial-rgb-value]
  (int (Math/round (* 255 (+ m initial-rgb-value)))))

(defn hsla-to-rgba
  "Converts an HSLA vector ```[H S L A]``` where H 0 to 360, S and L percentages
   to RGBA vector ```[R G B A]``` all 255
   and A remains unchanged (0 to 255)

   - hlsa: HSLA version of rgba input.  ```[H S L A]```
   - returns: vector of RGBA color. ```[R G B A]```
"
  [[hue saturation lightness alpha :as hlsa]]
  (let [c                (calc-c saturation lightness)
        x                (calc-x c hue )
        m                (- lightness (/ c 2))
        initial-rgb      (calc-initial-rgb c x hue)
        [red green blue] (map #(calc-rgb m %) initial-rgb)]
    [red green blue alpha]))


;; ============================= Color model keyword functions ===============

(defn model-from-to
  "Keywords used to define different color model types. Allows conversion form one model to another.
   Only currently supports ```:rgba``` and ```:hsla```

  - from-model: color model used by the input color value. (either ```:rgba``` or ```:hsla```)
  - to-model: color model to convert input color to. (either ```:rgba``` or ```:hsla```)
  - color: Color to be converted. Will be represented according to from-model. So either ```[R G B A]``` or ```[H S L A]```
  - returns: input color converted to the to-model. So either ```[R G B A]``` or ```[H S L A]```
"
  [from-model to-model color]
;;  (println "model-from-to(): from: " from-model " to:" to-model)
  (match [from-model to-model]
         [:rgba :hsla] (rgba-to-hsla color)
         [:hsla :rgba] (hsla-to-rgba color)
         [:rgba :rgba] color
         [:hsla :hsla] color
         :else (common/throw-illegal-arg (str "Unsupported color model conversion: " from-model " to " to-model))))


(defn model-ranges
  "Returns a vector defining the ranges of the values in the color model.
  eg ```:rgba``` would give ```[255 255 255 255]```
     ```:hsla``` would give ```[359 1 1 255]```

  - color-model: Color model for which ranges required. (either ```:rgba``` or ```:hsla```)
  - returns: vector of ranges for each dimension,  appropriate to color model. (either ```[255 255 255 255]``` or ```[359 1 1 255]```)
    Throws environment specific illegal argument exception for invalid color model.
"
  [color-model]
  (case color-model
    :rgba [255 255 255 255]
    :hsla [359 1 1 255]
    (common/throw-illegal-arg (str "Unsupported color model: " color-model))))

;; =========================== Color gradients =============================

(defn make-color-increment-fn
  "Uses the difference between the two colors to create a color increment function.

  - start-color: Color value as starting point for difference. either  ```[R G B]``` or ```[R G B A]```, or a packed int of form ```<alpha><R><G><B>```.
  - end-color: Color value as starting point for difference. either ```[R G B]``` or ```[R G B A]```, or a packed int of form ```<alpha><R><G><B>```.
  - returns: function taking a single color argument, and modifying it by the difference between start-color and end-color.
    Argument can be either  ```[R G B]``` or ```[R G B A]```, or a packed int of form ```<alpha><R><G><B>```.
    Result will be ```[R G B A]```

"
  [start-color end-color]
  ;;(println "make-color-increment-fn(): start-color: " start-color " end-color: " end-color)
  (let [start (color-as-rgba start-color)
        end (color-as-rgba end-color)
        difference (map #(- %2 %1) start end)]
;;    (println "make-color-increment-fn(): start: " start " end: " end)
    (fn [color]
      ;;(println "color-increment-fn(): color: " color " difference: " difference)
      (into [] (->> (color-as-rgba color)
                    (map #(+ %1 %2) difference)
                    (map #(min (max %1 0) 255)))))))

(defn color-difference-seq
  "Create a color sequence that follows on from the end color, using the start/end difference.
   Input colors in form of either Int32 or vector of RGBA

  - start-color: Color value as starting point for difference. either  ```[R G B]``` or ```[R G B A]```, or a packed int of form ```<alpha><R><G><B>```.
  - end-color: Color value as starting point for difference. either ```[R G B]``` or ```[R G B A]```, or a packed int of form ```<alpha><R><G><B>```.
  - count: Number of colors to generate using end-color as initial input and each resulting color as next input.
  - returns: vector of count colors, being a sequence after end-color, using the color difference between start-color and end-color to generate colors.
    All generated colors are in RGBA format ```[R G B A]```.
"
  [start-color end-color count]
  (let [inc-fn (make-color-increment-fn start-color end-color)]
    (take count (iterate inc-fn (inc-fn end-color)))))


;; =========================== Luminance and contrast =============================

(def ^:no-doc srgb-linear-threshold
  "sRGB (IEC 61966-2-1) encoded-value threshold below which a channel is on the linear segment."
  0.04045)

(defn ^:no-doc valid-channel?
  "True when the value is a number in the 0 to 255 channel range."
  [value]
  (and (number? value) (<= 0 value 255)))

(defn ^:no-doc validated-rgba
  "Normalises a color via [[color-as-rgba]], throwing an illegal argument exception when the color is
   not a packed int or a ```[R G B]``` / ```[R G B A]``` collection of 0 to 255 numbers.

  - color: packed int ```<alpha><R><G><B>``` or vector ```[R G B]``` / ```[R G B A]```
  - returns: vector ```[R G B A]```
"
  [color]
  (let [rgba (when (or (coll? color) (integer? color))
               (color-as-rgba color))]
    (if (and (sequential? rgba)
             (= 4 (count rgba))
             (every? valid-channel? rgba))
      rgba
      (common/throw-illegal-arg
       "Color must be a packed ARGB int or an [R G B] / [R G B A] vector of 0 to 255 values"))))

(defn ^:no-doc linearise-channel
  "Converts one gamma-encoded sRGB channel value (0 to 255) to linear light (0.0 to 1.0).

  - value: channel value from 0 to 255
  - returns: double linear channel value from 0.0 to 1.0
"
  [value]
  (let [c (/ (double value) 255.0)]
    (if (<= c srgb-linear-threshold)
      (/ c 12.92)
      (Math/pow (/ (+ c 0.055) 1.055) 2.4))))

(defn relative-luminance
  "Computes the WCAG 2.x relative luminance of a color: 0.0 for black up to 1.0 for white.

  Each channel is linearised with the sRGB transfer function
  (```c/12.92``` when ```c <= 0.04045```, else ```((c + 0.055)/1.055)^2.4```, with ```c = v/255```), then
  ```L = 0.2126 R + 0.7152 G + 0.0722 B```. The WCAG 2.x text quotes a threshold of 0.03928 rather than the
  sRGB 0.04045; no 8-bit channel value falls between the two, so the result is identical.

  Alpha is ignored: callers should composite a translucent color onto its background first.

  - color: packed int ```<alpha><R><G><B>```, or vector ```[R G B]``` or ```[R G B A]``` of 0 to 255 values.
    Normalised via [[color-as-rgba]].
  - returns: double relative luminance from 0.0 to 1.0.
    Throws an environment specific illegal argument exception for an invalid color.

```clojure
(relative-luminance [0 0 0 255])        => 0.0
(relative-luminance [255 0 0 255])      => 0.2126
(relative-luminance [128 128 128 255])  => 0.2158605001138992
(relative-luminance 0xFFFFFFFF)         => 1.0
```
"
  [color]
  (let [[red green blue] (map linearise-channel (validated-rgba color))]
    (+ (* 0.2126 red)
       (* 0.7152 green)
       (* 0.0722 blue))))

(defn contrast-ratio
  "Computes the WCAG 2.x contrast ratio between two colors: ```(L-lighter + 0.05) / (L-darker + 0.05)```
  where L is the [[relative-luminance]]. Symmetric in its arguments. Alpha is ignored.

  WCAG AA requires at least 4.5 for normal text (3 for large text); AAA requires 7 (4.5 for large text).

  - color-a: packed int ```<alpha><R><G><B>```, or vector ```[R G B]``` or ```[R G B A]``` of 0 to 255 values.
  - color-b: second color, in any of the same forms.
  - returns: double contrast ratio from 1.0 (identical luminance) to 21.0 (black against white).
    Throws an environment specific illegal argument exception for an invalid color.

```clojure
(contrast-ratio [0 0 0 255] [255 255 255 255])  => 21.0
(contrast-ratio [0 0 255] 0xFFFFFFFF)           => 8.592471358428805
(contrast-ratio [10 20 30] [10 20 30])          => 1.0
```
"
  [color-a color-b]
  (let [[darker lighter] (sort [(relative-luminance color-a)
                                (relative-luminance color-b)])]
    (/ (+ lighter 0.05)
       (+ darker 0.05))))

;;(color-as-rgba 4294179323)
;;(color-as-rgba 4294043879)
