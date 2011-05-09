(ns infer.classification
  "Fundamentals of classification, cross validation, and performance measures
   such as precision and recall.

   Classifiers are maps of classifier-name -> functions, data are maps of
   feature-name features."
  (:use infer.features
	infer.neighbors
	infer.linear-models
	[plumbing.core :only [map-map]]
	[clojure.contrib.map-utils :only [deep-merge-with]]
	[infer.probability :only [bucket +cond-prob-tuples]]))

(defn discretizing-classifiers
  "Makes a discretizing classifier out of each key-range pair."
  [ranges]
  (for [[k range] ranges]
    (bucket k range)))

;; TODO: Better handling for mismatch between fns keys and data keys.
(defn classify-one-to-one
  "Takes a map of fns and a map of features, where there is one classifier fn per feature and they share the same key names in the classifier and feature maps.  apply each classifer fn to the corresponding feature.

   => (classify-one-to-one
        {:a (present-when (gt 5)) :b (present-when (lt 5))}
        {:a 10 :b 5})
   {:a 1 :b 0}"
  [fns data]
  (into {}
	(for [[k f] fns
	      :let [v (data k)]]
	  [k (if (.contains (keys data) k)
	       (f v) 0)])))

(defn classify-one-to-each
  "Takes a map of fns and a map of features, apply each classifer fn to each feature in the data map individually.
 
   => (classify-one-to-each
        {:a (present-when (gt 5)) :b (present-when (lt 5))}
        {:a 10 :b 5})
   {:a {:a 1 :b 0} :b {:a 0 :b 0}}"
  [fns data]
  (into {} (for [[f-k f] fns] [f-k (map-map f data)])))

(defn classify-one-to-all
  "Takes a map of fns and a map of features, apply each classifer fn to the entire feature map.

  => (classify-one-to-each
       {:a (present-when #(and (> (:a %) 9) (< (:b %) 6)))}
       {:a 10 :b 5})
  {:a 1}"
  [fns data]
  (into {} (for [[f-k f] fns] [f-k (f data)])))

(defn classification-workflow
  "Composes a classification workflow from a classifier a counter and a
   transformer. Note that count-all has been abstracted due to the fact that you
   may count with reduce or merge-with depending on wheter you ahve vectors or
   maps."
  [transformer classifier count-all]
  (fn [obs] (count-all (map classifier (transformer obs)))))

(defn map-as-matrix [m]
  (let [ordered (map sort (vals (sort m)))]
	(map (comp vec vals) ordered)))

(defn real-precision [confusion-matrix]
  (map (fn [v i]
	 (/ (nth v i)
	    (apply + v)))
       confusion-matrix
       (range 0 (count confusion-matrix))))

(defn real-recall
  "Computes recall by class label from confusion matrix."
  [confusion-matrix]
    (real-precision (seq-trans confusion-matrix)))

(defn precision
  "Computes precision by class label from confusion matrix."
  [m]
    (real-precision (map-as-matrix m)))

(defn recall
  "Computes recall by class label from confusion matrix."
  [m]
    (real-precision (seq-trans (map-as-matrix m))))