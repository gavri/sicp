(ns e2.25 (:use clojure.test))
(def first-input '(1 3 (5 7) 9))
(def second-input '((7)))
(def third-input '(1 (2 (3 (4 (5 (6 7)))))))
(defn first-selector [x] (-> x rest rest first rest first))
(defn second-selector [x] (-> x first first))
(defn third-selector [x] (-> x rest first rest first rest first rest first rest first rest first))


(deftest first-test (is (= (first-selector first-input) 7)))
(deftest second-test (is (= (second-selector second-input) 7)))
(deftest third-test (is (= (third-selector third-input) 7)))

(run-all-tests #"e2.25")
