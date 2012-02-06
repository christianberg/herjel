(ns herjel.util.vector-math
  (:use midje.sweet))

(defn roughlies [expected]
  (fn [actual]
    (every? identity (map #((roughly %1) %2) expected actual))))

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

