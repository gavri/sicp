(ns e2.20 (:use clojure.test))
(defn filter' [p list]
  (cond
    (empty? list) '()
    (p (first list)) (cons (first list) (filter' p (rest list)))
    :else (filter' p (rest list))
    )
  )
(defn same-parity [reference & list]
  (def the-mod (mod reference 2))
  (defn has-same-parity [x] (= (mod x 2) the-mod))
  (cons reference (filter has-same-parity list))
  )

(deftest odd-test (is (= (same-parity 1 2 3 4 5 6 7) '(1 3 5 7))))
(deftest even-test (is (= (same-parity 2 3 4 5 6 7) '(2 4 6))))
(deftest first-degenerate-test (is (= (same-parity 1) '(1))))
(deftest second-degenerate-test (is (= (same-parity 1 2) '(1))))

(run-all-tests #"e2.20")
