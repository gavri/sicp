(defn sign [x y] (if (= (> x 0) (> y 0)) 1 -1))
(defn make-rat [x y]
  (list (* (sign x y) (Math/abs x))  (Math/abs y))
)
