(ns infer.uniclass
  {:doc "Uniclass Prediction from \"Online Passive Agressive Algorithms\"
   learn-radius function should do a brain-dead linear search to find a radius
    which contains target rate of the examples"}
  (:use [plumbing.core :only [map-map]]
	[infer.measures :only [sparse-vec-norm]]))

;; Sparse Passive Aggressive Uniclass

(defn diff-vec [x w]
  (merge-with + x (map-map (partial * -1) w)))

(defn update [alpha R w x]
  (let [diff (diff-vec w x)
	dist (sparse-vec-norm diff)
	loss (if (> dist R) (- dist R) 0.0)
	tau (min alpha loss)
	dist-inv (when (> dist 0.0) (/ 1.0 dist))]
    (when (> tau 0.0)
      (map-map (fn [v] (* v tau dist-inv)) diff))))

(defn learn-sparse-pa-iter
  [R w0 xs alpha]
  (reduce
    (fn [w x]
      (if-let [delta-w (update alpha R w x)]
	(merge-with + w delta-w)
	w))
    w0
    xs))

(defn learn-sparse-pa
  [R xs & {:keys [num-iters alpha] :or {num-iters 10 alpha 0.15}}]
  (loop [iter 0 w nil]
    (if (= iter num-iters)
      w
      (recur (inc iter) (learn-sparse-pa-iter R w xs alpha)))))

(defn num-errors [R w xs]
  (count
   (filter
    (fn [x] (> (sparse-vec-norm (diff-vec x w)) R))
    xs)))

(defn learn-radius
  [target xs &
   {:keys [num-iters,callback,step-mult] :or {num-iters 10, step-mult 0.1}}]
  (let [N (count xs)
	mean (->> xs
		  (reduce
		   (partial merge-with +)
		   {})
		  (map-map (fn [v] (/ v N))))
	init-R (+ (apply max
		       (map (fn [x] (sparse-vec-norm (diff-vec x mean)))
			    xs))
		  1.0e-4)]
    (assert (zero? (num-errors init-R mean xs)))
    (loop [w mean gamma 1.0 R init-R iter 0]
      (when callback
	(callback {:w w :gamma gamma :R R :iter iter}))
      (if (= iter num-iters)
	[w R gamma]	
	(let [scale (if (> gamma target)
		      (- 1.0 step-mult)
		      (+ 1.0 step-mult))
	      new-R (* scale R)
	      new-w (learn-sparse-pa new-R xs)			   
	      num-errs (num-errors new-R new-w xs)
	      new-gamma (double (/ (- N num-errs) N))]
	  (recur new-w new-gamma new-R (inc iter)))))))