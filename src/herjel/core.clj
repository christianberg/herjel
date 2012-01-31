(ns herjel.core
  (:use midje.sweet))

(unfinished )

(def camera-defaults
  {:origin [0 0 0]
   :viewport [[2 -1.5 1] [2 1.5 -1]]
   :resolution [[320 240]]})

(defn camera-rays
  "Returns a sequence of rays corresponding to pixels in a rectangular image."
  []
  )

(fact "")

(def black [0 0 0])
(def white [1 1 1])

(defn vecdot [v1 v2]
  (reduce + (map * v1 v2)))

(fact "vector dot product"
  (vecdot [1 0 0] [0 1 0]) => 0
  (vecdot [1 0 0] [1 0 0]) => 1
  (vecdot [1 2 3] [4 5 6]) => 32)

(defn vec- [& vs]
  (apply (partial map -) vs))

(fact "vector difference"
  (vec- [4 6 3] [2 9 1]) => [2 -3 2])

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