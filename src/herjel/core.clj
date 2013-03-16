(ns herjel.core
  (:import [java.awt.image BufferedImage]
           [javax.imageio ImageIO])
  (:use herjel.util.vector-math
        [clojure.java.io :only [file]]
        midje.sweet))

(def black [0 0 0])
(def white [1 1 1])

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

(defn sphere
  ([center radius]
     (sphere center radius white))
  ([center radius color]
     (fn [[origin direction]]
       (let [origin (vec- origin center)
             A (vecdot direction direction)
             B (* 2 (vecdot direction origin))
             C (- (vecdot origin origin) (* radius radius))
             x (- (* B B) (* 4 A C))]
         (when (>= x 0)
           [(first
              (filter
               (partial < 0) [(/ (+ B (Math/sqrt x)) (* -2 A))
                              (/ (- B (Math/sqrt x)) (* -2 A))])) (constantly color)])))))

(fact "ray doesn't hit sphere"
  ((sphere [0 2 0] 1) [[0 0 0] [1 0 0]]) => nil)

(fact "ray hits sphere head on"
      (first ((sphere [0 2 0] 1) [[0 0 0] [0 1 0]])) => (roughly 1))

(fact "ray grazes sphere"
      (first ((sphere [0 2 1] 1) [[0 0 0] [0 1 0]])) => (roughly 2))

(fact "ray hits sphere from inside"
      (first ((sphere [0 0 0] 1) [[0 0 0] [0 1 0]])) => (roughly 1))

(defn scene [& objects]
  (fn [ray]
    (if-let [[distance color] (some identity (sort (map #(% ray) objects)))]
      (color)
      black)))

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