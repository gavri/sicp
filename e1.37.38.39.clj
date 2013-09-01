;;;;;; e1.37 ;;;;;;;
(defn cont-frac-r [n d k]
  (defn cont-frac-r' [i]
    (if (> i k)
      0
      (/ (n i) (+ (d i) (cont-frac-r' (inc i))))
      )
    )
  (cont-frac-r' 1)
  )
(println (cont-frac-r (fn [i] 1.0) (fn [i] 1.0) 10))


; Clojure. So this iterative version isn't TCO. Doing it anyway because learning exercise

(defn cont-frac-i [n d k]
  (defn cont-frac-i' [i acc]
    (if (= i 0)
      acc
    (cont-frac-i' (dec i) (/ (n i) (+ (d i) acc)))
    ))
  (cont-frac-i' k 0)
  )
(println (cont-frac-i (fn [i] 1.0) (fn [i] 1.0) 10))

;;;;;; e1.38 ;;;;;;;
(defn euler-d [i]
  (if (= (rem (+ i 1) 3) 0)
  (* 2 (/ (+ i 1) 3))
  1
  )
)
(println (+ 2.0 (cont-frac-i (constantly 1) euler-d 10)))

;;;;;; e1.39 ;;;;;;;
(defn tan-cf-d [i] (- (* i 2) 1))
(defn tan-cf [x k]
  (defn tan-cf-n [i] (if (= i 1) x (- (* x x))))
  (cont-frac-i tan-cf-n tan-cf-d k)
)
