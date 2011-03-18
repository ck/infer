(ns infer.cluster
  (:use [clojure.contrib.seq-utils :only [indexed]]))

(defn get-cluster-sim [mode get-sim xs ys]
  (let [vals (for [x xs y ys] (get-sim x y))]
    (case mode
	  :avg (/ (reduce + vals) (count vals))
	  :max (apply max vals)
	  :min (apply min vals)
	  (throw (RuntimeException. (format "Unrecognized mode: %s" mode))))))

(defn get-pairwise-scores [score-fn xs]
  (into {}
    (for [[i x] (indexed xs)
	  [j y] (drop i (indexed xs))]
      [[i j] (score-fn x y)])))

(defn agglomerative-cluster
  "does bottom-up clustering between items xs using get-sim. each round
   two clusters are merged with maximum get-sim, as long as the max get-sim > 0.

   the score for two clusters merging is either the max, min, or avg between
   the elements of both clusters. initially, there is a singleton
   cluster for each element.

   mode arg should be one of
     :avg what's the avg similarity between clusters
     :max what's the max between elems of cluster
     :min what's the min between elems

  returns a clustering on xs represented as a list-of-lists e.g.
  [[:a :b] [:c]]"
  [mode get-sim xs]
  (let [cluster-sim (partial get-cluster-sim mode get-sim)]
    (loop [clusters (map (fn [x] [x]) xs)]
      (let [pairwise (get-pairwise-scores cluster-sim clusters)
	    [[i j] max-score] (apply max-key second pairwise)]
	(if (or (<= max-score 0.0) (= (count clusters) 1))
	  clusters
	  (recur
	   (->> (indexed clusters)
		(remove (comp #{i j} first))
		(map second)
		(cons (concat (nth clusters i) (nth clusters j))))))))))