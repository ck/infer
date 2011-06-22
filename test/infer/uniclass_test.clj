(ns infer.uniclass-test
  (:use infer.uniclass clojure.test))

(deftest diff-vec-test
  (is (=  {:b -1 :a 1.4 :c 1}
          (diff-vec {:a 1 :c 1} {:b 1 :a -0.4}))))

(deftest update-test
  (is (= {:a -0.5} (update 1.0 0.5 {:a 1.0} {:a 2.0}))))


(deftest fn-binary-search-test
  (let [[x fx]
	 (fn-binary-search
	  (fn [x] (- (* x x) 0.2))
	  0.0
	  1.0
	  10
	  1.0e-4)]
    (is (< (Math/abs (- x (Math/sqrt 0.2))) 1.0e-4))))

