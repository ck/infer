(ns infer.sparse-reranking
  ^{:doc "In sparse reranking each instance comes with several possible labels, but
    in contrast to classification, the labels are instance-specific, rather than fixed
    set of labels. So you can think of there being a global set of predicates rather than
    separate features for each label and predicate pair as in classification.

    Some terminology. A feat-vec is a map of active predicates for some choice for an instance.
    A datum is a seq of feat-vec representing feature maps for each choice for an instance.

    Another convention: During training, the first choice is the correct choice."
    
    }
  (:use [infer.sparse-classification :only [with-l2-regularization]]
	[infer.measures :only [sparse-dot-product]]
	[infer.core :only [log-add]]	
	[infer.optimize :only [remember-last lbfgs-optimize]]
	[plumbing.core :only [map-map apply-each]]))

(defn get-posteriors
  "returns [log-z posts] using a linear model over the weights
   posts is a sequence of posteriors for each choice in datum "
  [weights-map datum]
  (let [vals (map (partial sparse-dot-product weights-map) datum)		  
	log-val (log-add vals)]
    [log-val (map (fn [x] (Math/exp (- x log-val))) vals)]))


(defn get-datum
  "get-labels: Available choices for instance
   get-feat-extractor: Given instance, returns feat-vec for each label

  If you're making a datum for training, make sure your
  get-labels function puts the correct label first"
  [get-labels get-feat-extractor instance]
  (let [get-feats (get-feat-extractor instance)]
    (map get-feats (get-labels instance))))

(defn min-index [xs]
  (apply min-key (partial nth xs) (range (count xs))))

(defn max-index [xs]
  (apply max-key (partial nth xs) (range (count xs))))


;;; MaxEnt Reranking

(defn maxent-grad
  [posts datum]
  (->> posts
       (map-indexed (fn [i p] (if (zero? i) p (- p))))
       (vector datum)
       (apply map (fn [fv post] (map-map (partial * post) fv)))
       (reduce (partial merge-with (fnil + 0.0 0.0)))))

(defn maxent-obj-term [weight-map datum]
  (let [[log-z posts] (get-posteriors weight-map datum)
	true-log-score (sparse-dot-product weight-map (first datum))]
    [(- true-log-score log-z) (maxent-grad posts datum)]))

(defn maxent-obj [train-data preds weights]
  (let [weight-map (into {} (map vector preds weights))
	[obj-val grad-map]
	  (->> train-data
	       (map (partial maxent-obj-term weight-map))
	       (reduce (partial apply-each [+ (partial merge-with (fnil + 0.0 0.0))])))]    
    [(- obj-val) (map #(- (get grad-map % 0.0)) preds)]))

(defn train-maxent
  "train-data: seq of rerank datums
   the training data will be looped over multiple times
   returns the weight map that can be used with get-posteriors"
  [train-data & {:keys [sigma-sq] :or {sigma-sq 1.0}}]
  (let [preds (->> train-data (mapcat (partial mapcat keys)) (into #{}) seq)
	init-weights (repeat (count preds) 0.0)
	obj-fn (->> (partial maxent-obj train-data preds)
		    (partial with-l2-regularization sigma-sq)
		    remember-last)]
    (->> (lbfgs-optimize obj-fn  init-weights)
	 (map vector preds)
	 (into {}))))


;;; Mira Reranking

(defn mira-diff-vec-and-loss [weights y* y-hat losses datum]
  (let [delta-loss (- (nth losses y-hat) (nth losses y*))]
    (assert (> delta-loss 0))
    [(merge-with +
		(nth datum y*)
		(map-map - (nth datum y-hat)))
     delta-loss]))

(defn mira-update [decode weights losses datum]
  (let [y* (min-index losses)
	y-hat (decode weights losses datum)
	[delta-f delta-l] (mira-diff-vec-and-loss weights y* y-hat losses datum)
	weights-delta-f (sparse-dot-product weights delta-f)]
    (when (> delta-l 0)
      [(/ (- delta-l weights-delta-f)
	  (sparse-dot-product delta-f delta-f))
       delta-f])))

(defn mira-iter
  [init-weights get-losses get-datum instances
   {:keys [max-alpha]
    :or {max-alpha 0.15, num-iters 10}}]
  (reduce
     (fn [weights instance]
       (let [losses (get-losses instance)
	     datum (get-datum instance)
	     [alpha delta-f] (mira-update
			        (fn [weights losses datum]
				  (let [scores (map (fn [fv] (sparse-dot-product fv weights)) datum)]
				    (min-index scores)))
				weights
				losses
				datum)]
	 (if (nil? delta-f)
	     weights
	     (merge-with +
		      weights
		      (map-map (partial * (min alpha max-alpha)) delta-f)))))
     init-weights
     instances))

(defn train-mira
  [get-losses get-datum instances
   & {:keys [num-iters] :or {num-iters 10}}]
  (loop [weights {} iter 0]
    (if (= iter num-iters)
      weights
      (recur (mira-iter weights get-losses get-datum instances nil)
	     (inc iter)))))