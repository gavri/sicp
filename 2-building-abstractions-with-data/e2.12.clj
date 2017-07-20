(ns e2.12 (:use clojure.test))
(defn make-interval [x y] [x y])
(defn make-center-percent [c p] (make-interval (- c (* p c)) (+ c (* p c))))
(def lower-bound first)
(def upper-bound second)
(defn width [i] (- (upper-bound i) (lower-bound i)))
(defn center [i] (/ (+ (lower-bound i) (upper-bound i)) 2))
(defn percent [i] (/ (/ (width i) 2) (center i)))

(deftest percent-test (is (= (/ 1 9) (percent (make-interval 4 5)))))
(deftest make-center-percent-test (is (= (make-interval 3 5) (make-center-percent 4 (/ 1 4)))))

(run-all-tests #"e2.12")
