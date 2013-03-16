(ns example
  (:use herjel.core))

(defn go []
  (write-image "jpg" "example" 320 240
               (render (scene (sphere [-0.5 0 1.8] 1 [1 0 0])
                              (sphere [ 0.5 0 2] 1 [0 0 1])))))