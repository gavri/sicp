(defn average [x y] (/ (+ x y) 2))
(defn iterative-improve [good-enough? improve-guess]
  (fn ! [x guess]
    (def next-guess (improve-guess guess))
    (if (good-enough? next-guess)
      next-guess
      (! x next-guess)
      )))
(defn abs [n] (if (> n 0) n (- n)))

(defn sqrt [x]
  (defn good-enough? [guess]
    (< (abs (- x (* guess guess))) 0.001)
    )
  (defn improve-guess [guess]
    (average guess (/ x guess))
    )
  ((iterative-improve good-enough? improve-guess) x 1.0)
  )

(defn fixed-point [f]
  (defn good-enough? [guess]
    (< (abs (- guess (f guess))) 0.001)
    )
  (defn improve-guess [guess]
    (f guess)
    )
  ((iterative-improve good-enough? improve-guess) f 2.0)
  )
