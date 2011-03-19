(ns infer.cluster
  (:use [clojure.contrib.seq-utils :only [indexed]])
  (:require [infer.matrix :as matrix]))

(defn get-cluster-sim [mode get-sim xs ys]
  (let [vals (for [x xs y ys] (get-sim x y))]
    (case mode
	  :avg (/ (reduce + vals) (count vals))
	  :max (apply max vals)
	  :min (apply min vals)
	  (throw (RuntimeException. (format "Unrecognized mode: %s" mode))))))

(defn upper-triangle-pairs [n]
  (for [i (range n)
	j (range (inc i) n)]
    [i j]))

(defn get-pairwise-score-matrix [score-fn xs]
  (let [sim-matrix (matrix/fill 0.0 (count xs) (count xs))]
    (doseq [[i j] (upper-triangle-pairs (count xs))]
      (matrix/set-at
         sim-matrix
	 (score-fn (nth xs i) (nth xs j))
	 i j))
    sim-matrix))

(defn agglomerative-cluster
  "does bottom-up clustering between items using similarity maxtrix. each round
   two clusters are merged with maximum similarity, as long as the max sim > 0.

   the score for two clusters merging is either the max, min, or avg between
   the elements of both clusters. initially, there is a singleton
   cluster for each element.

   mode arg should be one of
     :avg what's the avg similarity between clusters
     :max what's the max between elems of cluster
     :min what's the min between elems

  returns a clustering on xs represented as a list-of-lists of indices.
  [[0 1] [2]]"
  [mode sim-matrix]
  (let [N (matrix/row-count sim-matrix)
	cluster-sim (partial get-cluster-sim mode
			     (fn [i j]
			       (matrix/get-at sim-matrix i j)))]
    (loop [clusters (map (fn [i] [i]) (range N))]
      (let [[i j] (apply max-key
			 (fn [[i j]] (cluster-sim (nth clusters i) (nth clusters j)))
			 (upper-triangle-pairs (count clusters)))
	    max-score (cluster-sim (nth clusters i) (nth clusters j))]
	(if (or (<= max-score 0.0) (= (count clusters) 1))
	  clusters
	  (recur
	   (->> (indexed clusters)
		(remove (comp #{i j} first))
		(map second)
		(cons (concat (nth clusters i) (nth clusters j))))))))))