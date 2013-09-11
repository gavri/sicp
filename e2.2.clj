(defn avg [x y] (/ (+ x y) 2))
; Point start
(defn make-point [x y] (list x y))
(defn x-point [p] (first p))
(defn y-point [p] (second p))
(defn print-point [p]
  (newline)
  (print "(")
  (print (x-point p))
  (print ",")
  (print (y-point p))
  (print ")")
  )
; Point End

; Segment Start
(defn make-segment [a b] (list a b))
(defn start-segment [s] (first s))
(defn end-segment [s] (second s))
(defn midpoint-segment [s] (make-point
                            (avg (x-point (start-segment s)) (x-point (end-segment s)))
                            (avg (y-point (start-segment s)) (y-point (end-segment s)))
                            ))
; Segment End
(def point-a (make-point 5 5))
(def point-b (make-point 10 10))
(def segment-c (make-segment point-a point-b))
(def midpoint-c (midpoint-segment segment-c))
(print-point midpoint-c)

