(ns infer.uniclass
  {:doc "Uniclass Prediction from \"Online Passive Agressive Algorithms\"
   learn-radius function should do a brain-dead linear search to find a radius
    which contains target rate of the examples"}
  (:use [plumbing.core :only [map-map sum]]
	[infer.measures :only [sparse-vec-norm]]))

;; Sparse Passive Aggressive Uniclass

(defn diff-vec [x w]
  (merge-with + x (map-map (partial * -1) w)))

(defn clip [x low high]
  (cond
   (< x low) low
   (> x high) high
   :else x))

(defn mean-vec [xs]
  (let [N (count xs)]
    (->> xs
	 (reduce
	  (partial merge-with +)
	  {})
	 (map-map (fn [v] (/ v N))))))

(defn update [alpha R w x]
  (let [diff (diff-vec x w)
	dist (sparse-vec-norm diff)
	loss (clip (- dist R) 0.0 alpha)]
    (when (> loss 0.0)
      (map-map (fn [v] (/ (* v loss) dist)) diff))))

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
  [R xs & {:keys [num-iters alpha] :or {num-iters 10 alpha 1}}]
  (loop [iter 0 w (mean-vec xs)]
    (if (= iter num-iters)
      w
      (recur (inc iter) (learn-sparse-pa-iter R w (shuffle xs) alpha)))))

(defn loss [R alpha w xs]
  (sum
   (fn [x] (let [d (sparse-vec-norm (diff-vec w x))]
	     (clip (- d R) 0.0 alpha)))
   xs))

(defn num-errors [R w xs]
  (count
   (filter
    (fn [x] (> (sparse-vec-norm (diff-vec w x)) R))
    xs)))

(defn fn-binary-search
  "assume f is monotonic on [low,high], want to find f(x)
   closest to 0 within max number of steps"
  [f low high num-steps tol]
  (let [mid (/ (+ low high) 2)
	f-mid (f mid)]
    (cond
     (or (zero? num-steps) (< (Math/abs f-mid) tol)) [mid f-mid]
     (> f-mid 0) (recur f low mid (dec num-steps) tol)
     (< f-mid 0) (recur f mid high (dec num-steps) tol))))

(defn learn-radius
  [target xs &
   {:keys [num-iters,callback,step-mult] :or {num-iters 10, step-mult 0.1}}]
  (let [N (count xs)
	mean (mean-vec xs)
	max-R (+ (apply max
		       (map (fn [x] (sparse-vec-norm (diff-vec x mean)))
			    xs))
		  1.0e-4)
	_ (assert (zero? (num-errors max-R mean xs)))
	last-w (atom nil)
	f (fn [R] (let [w (learn-sparse-pa R xs)
			num-errs (num-errors R w xs)
			gamma (/ (- N num-errs) N)]
		    (reset! last-w w)
		    (- gamma target)))
	[R dist] (fn-binary-search f 0.0 max-R 10 1.0e-4)]
    ;; binary search 
    [@last-w R (+ dist target)]))