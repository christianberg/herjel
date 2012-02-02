(ns herjel.core
  (:import [java.awt.image BufferedImage]
           [javax.imageio ImageIO])
  (:use [clojure.java.io :only [file]]
        midje.sweet))

(defn roughlies [expected]
  (fn [actual]
    (every? identity (map #((roughly %1) %2) expected actual))))

(def black [0 0 0])
(def white [1 1 1])

(defn vec+ [& vs]
  (apply map + vs))

(fact "vector addition"
  (vec+ [1 2 3]) => [1 2 3]
  (vec+ [1 2 3] [4 5 6]) => [5 7 9]
  (vec+ [0 0 1] [0 1 0] [1 0 0]) => [1 1 1])

(defn scale [scalar vector]
  (map (partial * scalar) vector))

(fact "scaling a vector"
  (scale 2 [3 5 9]) => [6 10 18]
  (scale -0.5 [0 20 -8.4]) => [0.0 -10.0 4.2])

(defn vecdot [v1 v2]
  (reduce + (map * v1 v2)))

(fact "vector dot product"
  (vecdot [1 0 0] [0 1 0]) => 0
  (vecdot [1 0 0] [1 0 0]) => 1
  (vecdot [1 2 3] [4 5 6]) => 32)

(defn veclen [v]
  (Math/sqrt (vecdot v v)))

(fact "vector length"
  (veclen [1 0 0]) => 1.0
  (veclen [3 4 0]) => 5.0)

(defn vec- [& vs]
  (apply (partial map -) vs))

(fact "vector difference"
  (vec- [4 6 3] [2 9 1]) => [2 -3 2])

(defn norm [v]
  (scale (/ (veclen v)) v))

(fact "normalize vector"
  (norm [1.0 0.0 0.0]) => [1.0 0.0 0.0]
  (norm [0 2 0]) => [0.0 1.0 0.0]
  (norm [3 0 4]) => (roughlies [0.6 0.0 0.8]))

(def camera-defaults
  {:origin [0 0 0]
   :viewport [[-2 1.5 2] [2 1.5 2] [2 -1.5 2]]
   :x-resolution 320})

(defn camera-rays
  "Returns a sequence of rays corresponding to pixels in a rectangular image."
  ([] (camera-rays camera-defaults))
  ([{:keys [origin viewport x-resolution]}]
     (let [[upper-left upper-right lower-right] viewport
           width (vec- upper-right upper-left)
           height (vec- lower-right upper-right)
           ratio (/ (veclen width) (veclen height))
           y-resolution (int (/ x-resolution ratio))
           dx (scale (/ x-resolution) width)
           dy (scale (/ y-resolution) height)
           start (vec+ upper-left (scale 0.5 dx) (scale 0.5 dy))]
       (for [y (range y-resolution)
             x (range x-resolution)]
         [origin (norm (vec+ start (scale x dx) (scale y dy)))]))))

(fact "camera ray tests"
  (let [options {:origin [0 0 0]
                 :viewport [[-1 1 1] [1 1 1] [1 -1 1]]}
        a (/ 0.5 (Math/sqrt 1.5))
        b (/ -0.5 (Math/sqrt 1.5))
        c (/ 1 (Math/sqrt 1.5))]
    (camera-rays (assoc options :x-resolution 1)) => [[[0 0 0] [0.0 0.0 1.0]]]
    (camera-rays (assoc options :x-resolution 2)) => [[[0 0 0] [b a c]]
                                                      [[0 0 0] [a a c]]
                                                      [[0 0 0] [b b c]]
                                                      [[0 0 0] [a b c]]]
    (count (camera-rays)) => (* 320 240)))

(defn sphere [center radius]
  (fn [origin direction]
    (let [origin (vec- origin center)
          A (vecdot direction direction)
          B (* 2 (vecdot direction origin))
          C (- (vecdot origin origin) (* radius radius))
          x (- (* B B) (* 4 A C))]
      (when (>= x 0)
        (first
         (filter
          (partial < 0) [(/ (+ B (Math/sqrt x)) (* -2 A))
                         (/ (- B (Math/sqrt x)) (* -2 A))]))))))

(fact "ray doesn't hit sphere"
  ((sphere [0 2 0] 1) [0 0 0] [1 0 0]) => nil)

(fact "ray hits sphere head on"
  ((sphere [0 2 0] 1) [0 0 0] [0 1 0]) => (roughly 1))

(fact "ray grazes sphere"
  ((sphere [0 2 1] 1) [0 0 0] [0 1 0]) => (roughly 2))

(fact "ray hits sphere from inside"
  ((sphere [0 0 0] 1) [0 0 0] [0 1 0]) => (roughly 1))

(defn scene []
  (fn [ray] black))

(fact "an empty scene doesn't intersect a ray"
  ((scene) ...ray...) => black)

(defn render
  "Returns a sequence of color vectors, each of which is a result of
  passing a camera ray to the scene function."
  [scene]
  (map scene (camera-rays)))

(fact
  (render (scene)) => [black]
  (provided
    (camera-rays) => [[[0 0 0] [1 0 0]]]))

(defn rgb-to-int [rgb]
  (reduce +
          (map bit-shift-left
               (map #(int (* 255 %)) rgb)
               [16 8 0])))

(fact
  (rgb-to-int [0 0 0]) => 0
  (rgb-to-int [1 1 1]) => (dec (* 256 256 256)))

(defn write-image [type filename width height pixels]
  (let [image (BufferedImage. width height BufferedImage/TYPE_INT_RGB)
        ints (into-array Integer/TYPE (map rgb-to-int pixels))]
    (.setRGB image 0 0 width height ints 0 width)
    (ImageIO/write image type (file (str filename "." type)))))