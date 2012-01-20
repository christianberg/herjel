(ns herjel.core
  (:use midje.sweet))

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

(defn intersect [ray sphere]
  (let [{:keys [center radius]} sphere
        {:keys [origin direction]} ray
        origin (vec- origin center)
        A (vecdot direction direction)
        B (* 2 (vecdot direction origin))
        C (- (vecdot origin origin) (* radius radius))
        x (- (* B B) (* 4 A C))]
    (when (>= x 0)
      (first
       (filter
        (partial < 0) [(/ (+ B (Math/sqrt x)) (* -2 A))
                       (/ (- B (Math/sqrt x)) (* -2 A))])))))

(fact "ray doesn't hit sphere"
  (intersect {:origin [0 0 0] :direction [1 0 0]}
             {:center [0 2 0] :radius 1}) => nil)

(fact "ray hits sphere head on"
  (intersect {:origin [0 0 0] :direction [0 1 0]}
             {:center [0 2 0] :radius 1}) => (roughly 1))
(fact "ray grazes sphere"
  (intersect {:origin [0 0 0] :direction [0 1 0]}
             {:center [0 2 1] :radius 1}) => (roughly 2))
(fact "ray hits sphere from inside"
  (intersect {:origin [0 0 0] :direction [0 1 0]}
             {:center [0 0 0] :radius 1}) => (roughly 1))