(ns herjel.util.vector-math
  (:use midje.sweet))

(defprotocol VectorMath
  "3D vector mathematical functions."
  (vec+ [v v2] "Add vectors.")
  (vec- [v v2] "Subtract vectors.")
  (scale [v scalar] "Multiply a vector by a scalar.")
  (vecdot [v1 v2] "Vector dot product.")
  (veclen [v] "Vector length.")
  (norm [v] "Return a normalized vector (with unit length)."))

(defrecord Vector3 [#^double x  #^double y  #^double z]
  VectorMath
  (vec+ [this v2]
    (Vector3. (+ x (:x v2)) (+ y (:y v2)) (+ z (:z v2))))
  (vec- [this v2]
    (Vector3. (- x (:x v2)) (- y (:y v2)) (- z (:z v2))))
  (scale [this scalar]
    (Vector3. (* x scalar) (* y scalar) (* z scalar)))
  (vecdot [this v2]
    (+ (* x (:x v2)) (* y (:y v2)) (* z (:z v2))))
  (veclen [this]
    (Math/sqrt (+ (* x x) (* y y) (* z z))))
  (norm [this]
    (scale this (/ (veclen this)))))

(fact "vector addition"
  (vec+ (Vector3. 1 2 3) (Vector3. 4 5 6)) => (Vector3. 5 7 9)
  (vec+ (Vector3. 0 0 1) (Vector3. 1 1 0)) => (Vector3. 1 1 1))

(fact "vector difference"
  (vec- (Vector3. 4 6 3) (Vector3. 2 9 1)) => (Vector3. 2 -3 2))

(fact "scaling a vector"
  (scale (Vector3. 3 5 9) 2) => (Vector3. 6 10 18)
  (scale (Vector3. 0 20 -8.4) -0.5) => (Vector3. 0.0 -10.0 4.2))

(fact "vector dot product"
  (vecdot (Vector3. 1 0 0) (Vector3. 0 1 0)) => 0.0
  (vecdot (Vector3. 1 0 0) (Vector3. 1 0 0)) => 1.0
  (vecdot (Vector3. 1 2 3) (Vector3. 4 5 6)) => 32.0)

(fact "vector length"
  (veclen (Vector3. 1 0 0)) => 1.0
  (veclen (Vector3. 3 4 0)) => 5.0)

(fact "normalize vector"
  (norm (Vector3. 1.0 0.0 0.0)) => (Vector3. 1.0 0.0 0.0)
  (norm (Vector3. 0 2 0)) => (Vector3. 0.0 1.0 0.0))

