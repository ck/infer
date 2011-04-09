(ns infer.cluster
  (:use [clojure.contrib.seq-utils :only [indexed]])
  (:require [infer.matrix :as matrix]))

(defn get-cluster-sim [mode get-sim xs ys]
  (let [vals (for [x xs y ys] (get-sim x y))]
    (case mode
	  :avg (/ (reduce + vals) (count vals))
	  :max (apply max vals)
	  :min (apply min vals)
	  :median (nth (sort vals) (/ (count vals) 2))
	  (throw (RuntimeException. (format "Unrecognized mode: %s" mode))))))

(defn best-agglomerative-merge [get-cluster-sim clusters]
  (let [cs (-> clusters seq indexed vec)]
    (first  (apply max-key
		   second
		   (for [[i e1] cs
			 [j e2] (subvec cs (inc i))]
		     [[i j] (get-cluster-sim e1 e2)])))))

(defn cluster-merge [clusters [i j]]
  (assert (< i j))
  (->> (indexed clusters)
       (remove (comp #{i j} first))
       (map second)
       (cons (vec (concat (nth clusters i) (nth clusters j))))
       vec))

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
  [mode get-sim items]
  (let [cluster-sim (partial (memoize get-cluster-sim) mode get-sim)]
    (loop [clusters (vec (map vector items))]
      (if (= (count clusters) 1)
	clusters
	(let [to-merge (best-agglomerative-merge cluster-sim clusters)
	      max-score (cluster-sim (nth clusters (first to-merge))
				     (nth clusters (second to-merge)))]
	  (if (<= max-score 0.0)
	    clusters
	    (recur (cluster-merge clusters to-merge))))))))