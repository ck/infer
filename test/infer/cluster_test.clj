(ns infer.cluster-test
  (:use clojure.test infer.cluster infer.matrix))

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

(deftest agglomerative-cluster-test
  (let [S (matrix [[0 1 0]
		   [1 0 0.5]
		   [0 0.5 0]])]
    (is (= [[0 1] [2]]
	     (agglomerative-cluster :min (fn [i j] (get-at S i j)) (range 3))))))

(defn agglomerative-perf [S items]
  (time (agglomerative-cluster
	 :min (fn [i j] (get-at S i j)) (range items))))