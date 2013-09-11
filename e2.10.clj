(defn make-interval [a b] [a b])
(def lower-bound first)
(def upper-bound second)
(defn mul-interval [x y]
  (def p1 (* (lower-bound x) (lower-bound y)))
  (def p2 (* (lower-bound x) (upper-bound y)))
  (def p3 (* (upper-bound x) (lower-bound y)))
  (def p4 (* (upper-bound x) (upper-bound y)))
  (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4)))
(defn div-interval [x y]
  (if (= (lower-bound y) (upper-bound y)) (throw (new Exception "You can't divide by zero!")))
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
