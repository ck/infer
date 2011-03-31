(ns infer.sparse-classification
  {:doc "Implements sparse classification algorithms. This ns in contrast
    to infer.{learning,linear-models} is geared towards classifying sparse feature
    vectors rather than dense matrix encoding. The dense encoding
    isn't currently great for large feature spaces (like you get in NLP problems).
    The classifiers here should work for low millions of features and lots of data
    (assuming each datum has few number of non-zero feature);
    uses infer.optimize to find parameters to maximize objectives, which 
    also scales well with lots of data. Objective function computation
    is parallelized. 
    
    Each datum in this ns is represented by a map from feature to value. For instance,
    for doing category labeling of a document you might have a datum like:
    {
      \"has-word='Obama'\": 1
      \"from-base-url='cnn.com'\": 1
      \"referring-tweet-hashtag='#politics'\": 1
    }    
    
    Currently implemented:
    - logistic regression (maximum entropy classification)
    - online learning (MIRA)
    
    To serialize a classifier, use serialize-obj to get a data structure
    that can be serialized and load-classifier to return
    a ISparseClassifier protocol
            
    See test for example usage."
   :author "Aria Haghighi <me@aria42.com>"}
  (:use [plumbing.core :only [apply-each map-map]]
        [infer.core :only [log-add]]
        [infer.measures :only [dot-product, sparse-dot-product]]
        [infer.optimize :only [remember-last, lbfgs-optimize]]
        [clojure.contrib.map-utils :only [deep-merge-with]]))

;; ---------------------------------------------
;; Core abstractions
;; ---------------------------------------------

(defprotocol ISparseClassifier
  (labels [model] "Returns set of possible labels for task.")
  (predict-label [model datum] "Predict highest-scoring label.")
  (serialize-obj [_] "A data structure that can be serialized and read"))

(defprotocol ISparseProbabilisticClassifier
  (label-posteriors [model datum]
                    "Return map of label -> posterior for datum,
                     which is a map from active feature to value."))

(defprotocol ISparseOnlineClassifier
  (update-model [model label datum]
                "Online classifier can be updated with
                 single example without training."))

(declare read-classifier)

;; ---------------------------------------------
;; Data indexing
;; ---------------------------------------------

(defn- index-data
  "Takes seq of [label datum] pairs and returns 
   sets of all labels and predicates (a.k.a. features)."
  [labeled-data]
  (let [labels (into #{} (map first labeled-data))
        preds (loop [labeled-data labeled-data preds (transient #{})]
                (if (empty? labeled-data) (persistent! preds)
                    (recur (rest labeled-data) 
                           (reduce conj! preds (-> labeled-data first second keys)))))]
    [preds labels]))

(defn- encode-weights-to-map
  "Converts flat seq of weights (model params) into nested map label => pred => weight.
   Assumes weights have been partitioned by labels. Uses ordering from
   labels and preds to interpret weights."
  [weights labels preds]
  (let [all-label-weights (partition-all (count preds) weights)]
    (->> (zipmap labels all-label-weights) ; label -> label weights
         (map-map                          ; label -> (pred -> weight)
          (fn [label-weights]
            (zipmap preds label-weights))))))

(defn- decode-map-to-weights
  "Converts nested map label => pred => weight to flat seq
   of weights for optimization."
  [weight-map labels preds]
  (apply concat
         (for [label labels :let [label-weights (weight-map label)]]
           (map label-weights preds))))

;; -----------------------------
;; Linear classifier
;; -----------------------------

(defn- make-score-fn
  "Returns map label -> unnormalized log-probability of label."
  [weights-map]
  (fn [datum]
    (map-map
     (fn [weights] (sparse-dot-product datum weights))        
     weights-map)))

(defn- make-posterior-fn
  "Return fn: label -> [logZ posterior-map], where
   posterior-map label -> posterior."
  [weights-map]
  (let [score-fn (make-score-fn weights-map)]
    (fn [datum]
      (let [labels (keys weights-map)
            scores (score-fn datum)
            log-z (log-add (vals scores))]
        [log-z
         (map-map #(Math/exp (- % log-z)) scores)]))))

(defrecord SparseLinearClassifier [weights-map]
  ISparseClassifier
  (labels [this] (keys weights-map))
  (predict-label [this datum]
                 (apply max-key ((make-score-fn weights-map) datum) (labels this)))
  (serialize-obj [this]
                 {:data weights-map :type :linear})

  ISparseProbabilisticClassifier
  ;; If you have a linear classifier you use
  ;; soft-max for probabilities. This is correct
  ;; for logistic regression but is well-defined
  ;; for any linear classifier.
  (label-posteriors [this datum]
                    ((comp second (make-posterior-fn weights-map)) datum)))

;; ---------------
;; Regularization
;; ----------------

(defn with-l2-regularization
  "obj-fn: fn from weights to [obj-val grad]
   weights and grad are seqs of doubles"
  [sigma-sq obj-fn weights]
  (if (<= sigma-sq 0) (obj-fn weights)
      (let [l2-val (/ (dot-product weights weights) (* 2 sigma-sq))
	    [val grad] (obj-fn weights)]
	[(+ val l2-val)
	 (map (fn [grad-val weight-val]
		(+ grad-val (/ weight-val sigma-sq)))
	      grad weights)])))

;; ---------------------------------------------
;; Logistic Regression
;; ---------------------------------------------

(defn- log-reg-update [labels post-fn gold-label datum]
  (let [post-res (post-fn datum)
        log-z (first post-res)
        posts (second post-res)
        empirical (reduce
                   (fn [res [k v]]
                     (assoc-in res [gold-label k] v))
                   {}
                   datum)
        expected (map-map
                  (fn [post]
                    (map-map 
                     (fn [v] (* v post))
                     datum))
                  posts)]
    [(- (Math/log (posts gold-label)))
     (deep-merge-with - expected empirical)]))

(defn- make-log-reg-obj-fn
  [labels post-fn]
  (fn [labeled-data]
    (loop [labeled-data labeled-data sum 0.0 grad {}]
      (if (empty? labeled-data)
        [sum grad]
        (let [label (-> labeled-data first first)
              datum (-> labeled-data first second)
              [log-like local-grad] (log-reg-update labels post-fn label datum)]
          (recur (rest labeled-data)
                 (+ sum log-like)
                 (deep-merge-with + grad local-grad)))))))

(defn- log-reg-parallel-compute
  [labels post-fn labeled-data]
  (let [obj-fn (make-log-reg-obj-fn labels post-fn)]
    (->> labeled-data
         (partition-all 200)
         (pmap obj-fn)
         (reduce
          (fn [[sum-log-like sum-grad] [log-like grad]]
            [(+ sum-log-like log-like)
             (deep-merge-with + sum-grad grad)])
          [0.0 {}]))))

(defn learn-logistic-regression
  "Returns fn: datum => posterior, where the posterior is a map
   from label to probability. You can get a classifier by composing
   the function with (fn [m] (first (apply max-key second m))).
   
   Each datum is simply a map from predicates (features) to value 
   (for binary features, this is just 1). All absent features assumed to be 0.

   labeled-data-source: either a seq of [label datum] representing labeled
   data or a no-arg callback to return seq of [label datum].
   Set of possible labels implicitly determined from labeled data.

   Main option is sigma-squared (L2 regularization parameter). All options
   passed on to optimization (lbfgs-optimize in infer.optimize).
      
   TODO: Support L1 regularization and feature pruning"
  [labeled-data-source &
   {:as opts
    :keys [sigma-squared, max-iters]
    :or {sigma-squared 1.0 max-iters 100}}]
  (let [labeled-data-fn (if (or (seq? labeled-data-source) (vector? labeled-data-source))
                          (constantly labeled-data-source)
                          labeled-data-source)
        [preds labels] (index-data (labeled-data-fn))
        init-weights (double-array (* (count preds) (count labels)))
        obj-fn (->> (fn [weights]
		      (let [weight-map (encode-weights-to-map weights labels preds)
			[log-like grad-map]
			(log-reg-parallel-compute labels
						  (make-posterior-fn weight-map)
						  (labeled-data-fn))
			grad (decode-map-to-weights grad-map labels preds)]
			[log-like grad]))
		    (partial with-l2-regularization sigma-squared)
		    remember-last)
        weights (apply lbfgs-optimize obj-fn init-weights (-> opts seq flatten))
        weight-map (encode-weights-to-map weights labels preds)]
    (SparseLinearClassifier. weight-map)))


;; ---------------------------------------------
;; MIRA Online Learning
;; See http://aria42.com/blog/?p=216
;; ---------------------------------------------

(defn- add-scaled
  "x <- x + scale * y
  Bottleneck: written to be efficient"
  [x scale y]
  (persistent!
   (reduce
    (fn [res elem]
      (let [k (first elem) v (second elem)]
        (assoc! res k (+ (get x k 0.0) (* scale v)))))
    (transient x)
    y)))

(defn- mira-weight-update
  "Returns new weights assuming error predict-label instead of gold-label.
   delta-vec is the direction and alpha the scaling constant."
  [weight-map delta-vec gold-label predict-label alpha]
  (update-in weight-map [gold-label] add-scaled alpha delta-vec))

(defn- update-mira
  "Update mira for an example returning [new-mira error?]."
  [mira gold-label datum]
  (let [predict-label (predict-label mira datum)]
    (if (= predict-label gold-label)
      ;; If we get it right do nothing
      [mira false]
      ;; otherwise, update weights
      (let [score-fn ((make-score-fn (:weights-map mira)) datum)
            loss (-> mira :losses  (get [gold-label predict-label] 0.0))
            gap (- (get score-fn gold-label 0.0) (get score-fn predict-label 0.0))
            alpha  (/ (- loss  gap) (* 2 (sparse-dot-product datum datum)))
            new-mira (-> mira
                         ;; Update Current Weights
                         (update-in [:weights-map]
                                    mira-weight-update datum gold-label
                                    predict-label alpha))]
        [new-mira true]))))

(defrecord MiraOnlineClassifier [weights-map losses]
  ISparseClassifier
  (labels [this] (into #{} (keys weights-map)))
  (predict-label [this datum]
                 (let [label-scores ((make-score-fn weights-map) datum)]
                   (apply max-key label-scores  (labels this))))
  (serialize-obj [this]
                 {:data (into {} this)
                  :type :mira})

  ISparseOnlineClassifier
  (update-model [this label datum]
                (first (update-mira this label datum))))

(defn new-mira
  "Returns MIRA online classifier.  You manually
   call update-model (see ISparseOnlineClassifier)
   to train it as data comes in. You can simulate
   batch learning by just calling update-model
   on all your data.

   Losses: map of [true-label guess-label] to loss
   for that prediction; a missing entry will be assumed
   to have 0 loss (which should only be when (= true-label guess-label))
   By default for a set labels you should do:
   (reduce
     (fn [res [true-label guess-label]]
       (if (= true-label guess-label)
          res
          (assoc res [true-label guess-label] 1.0)))
      {}
      label)

   Unfortunately this isn't Averaged MIRA which
   can't be updated efficiently in true online
   fashion for large feature sets."
  [losses]
  (let [label-pairs (keys losses)
        labels (into #{} (concat (map first label-pairs) (map second label-pairs)))
        init-weights (reduce (fn [res label] (assoc res label {})) {} labels)]
    (MiraOnlineClassifier. init-weights losses)))


;; ---------------------------------------------
;; Serialization
;; ---------------------------------------------

(defn load-classifier
  "loads classifier from a clj data object that
  came from serialize-obj."
  [obj]
  (case (:type obj)
        :linear (SparseLinearClassifier. (:data obj))
        :mira (let [{:keys [weights-map, losses]} (:data obj)]
                (MiraOnlineClassifier. weights-map losses))))
