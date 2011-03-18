(ns infer.cluster-test
  (:use clojure.test infer.cluster))

(deftest cluster-sim-test
  (let [sim (fn [a b]
	      (case [a b]
		[:a :b] 1
		[:b :c] 0.5
		[:a :c] 0.2
		0.0))]
    (is (= (get-cluster-sim :max sim [:a] [:b]) 1))
    (is (= (get-cluster-sim :avg sim [:a] [:b :c]) 0.6))
    (is (= (get-cluster-sim :min sim [:a] [:b :c]) 0.2))))

(deftest get-pairwise-scores-test
  (let [sim (fn [a b]
	      (case [a b]
		[:a :b] 1
		[:b :c] 0.5
		[:a :c] 0.2
		0.0))
	cluster-score (partial get-cluster-sim :avg sim)]
    (is (= {[0 0] 0.25, [0 1] 0.425, [0 2] 0.35, [1 1] 0.125, [1 2] 0.25, [2 2] 0.0}
	   (get-pairwise-scores cluster-score [[:a :b] [:b :c] [:c]])))))

(deftest agglomerative-cluster-test
  (let [sim (fn [a b]
	      (case [a b]
		[:a :b] 1
		[:b :c] 0.5
		[:a :c] 0.0
		0.0))]
    (is (= [[:a :b] [:c]]
	  (agglomerative-cluster :min sim [:a :b :c])))))