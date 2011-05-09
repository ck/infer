(ns  infer.learning
  (:use clojure.set
	clojure.contrib.math
	infer.core
	infer.matrix
	infer.measures
	infer.probability
	infer.information-theory
	infer.features
	[plumbing.core :only [max-by]]))

;;optimization, regularization, and subset selection
;;TODO: should be split into a few libs

(defn feature-target-pairs
([A] (feature-target-pairs A (- (column-count A) 1)))
([A target]
    (pmap (fn [feature]
	    (select-columns A [feature target]))
	  (remove (eq target)
			  (range 0 (column-count A))))))

(def mi-from-matrix
     (comp
      #(mutual-information (first %) (rest %))
      joint-and-marginals-from-vectors
      from-matrix))

(defn feature-target-mi [A]
  (pmap mi-from-matrix 
       (feature-target-pairs A)))

(defn feature-mi-matrix [A]
  (pmap
   #(pmap mi-from-matrix (feature-target-pairs A %))
   (range 0 (column-count A))))

(defn index-of-max [v indices]
 (second (max-by first (map vector v indices))))

;;watch out for the ordering of columns that are selected from the matrix for the mi matrix.
(defn mrmr-feature-set
"k: # of features to select
t: index of target
vecs: feature-target vectors" 
[k t vecs]
(let [A (matrix vecs)
      AI (feature-mi-matrix A)
      Ixy (nth AI t)
      fs (remove (eq t) (range 0 (count AI)))
      initial (index-of-max Ixy fs)
      S* [initial]
      mrmr (fn [S]
;;repeat until s = k
	     (if (= k (count S)) S
;;find max: ( I(xi,y) - 1/S * sum of I(xi, xj))
	     (let [xis (difference (into #{} fs) (into #{} S))
		   goal (map 
			 (fn [xi]
			   (- (nth Ixy xi)
			      (* (/ 1 (count S))
				 (sum (map #(nth
					    (nth AI %)
					    xi)
					   S)))))
			   xis)]
;;move best into set of selected
		   (recur (conj S (index-of-max goal xis))))))]
  (mrmr S*)))

(defn coordinate-convergence? [precision Bnew Bold]
(any? true? 
	   (map (fn [bnew bold]
		   (and
		    (not (= 0 bold))
		    (< (abs (- bnew bold)) precision)))
		Bnew
		Bold)))

(defn euclidean-convergence? [precision Bnew Bold]
    (<= (euclidean-distance Bnew Bold)
	    precision))

(defn active-set [B]
  (filter (complement nil?)
  (map (fn [b i] (if (not (= 0 b)) i nil))
       B
       (range 0 (count B)))))

(defn active-set-convergence? [Bnew Bold]
  (= (active-set Bnew)
     (active-set Bold)))

(defn coordinate-descent
  ([f Bold converged? j]
     (coordinate-descent f 1 Bold converged? j))
  ([f n Bold converged? j]
     (let [Bold-snapshot (copy-matrix Bold) ;;WARNING: Bold is mutated duirng the inner loop.
	   Bnew  (f Bold j)]
       (if (converged?
		(from-column-matrix Bnew)
		(from-column-matrix Bold-snapshot))
	 {:betas Bnew :iterations n}
	 (recur f (+ n 1) Bnew converged? j)))))

(defn dydx [f x1 x0]
  (let [fx1 (apply f x1)
	fx0 (apply f x0)
	dy (- fx1 fx0)]
    (map #(/ dy
	     (- %1 %2))
	 x1 x0)))

(defn update-xs [f step x1 x0]
(map #(- %1 (* step %2)) x1 (dydx f x1 x0)))

(defn gradient-descent
"http://en.wikipedia.org/wiki/Gradient_descent

A more robust implementation of the algorithm would also check whether the function value indeed decreases at every iteration and would make the step size smaller otherwise. One can also use an adaptive step size which may make the algorithm converge faster.
"
([f step precision x]
   (let [xs (if (vector? x) x [x])]
   (gradient-descent f step precision xs (repeat (count xs) 0))))
([f step precision x1 x0]
   (if (<= (euclidean-distance x1 x0) precision) x1
    (recur f step precision
	   (update-xs f step x1 x0) x1))))

(defn update-guesses [f loss step x1 e1]
(map #(- %1 (* step %2)) x1 (loss f x1 e1)))

(defn sgd
"http://en.wikipedia.org/wiki/Stochastic_gradient_descent"
([f loss step precision guesses examples]
   (sgd f loss step precision guesses
	(repeat (count guesses) 0)
	examples))
([f loss step precision x1 x0 examples]
   (if (<= (euclidean-distance x1 x0) precision) x1
    (recur f loss step precision
	   (update-guesses f loss step x1
			   (first examples))
	   x1 (rest examples)))))

;; (defn dloss-dx [f loss x1 x0 e1]
;;   (let [fx1 (loss (f x1 (vec-but-last e1)) (vec-last e1))
;; 	fx0 (loss (f x0 (vec-but-last e1)) (vec-last e1))
;; 	dy (- fx1 fx0)]
;;     (map #(/ dy
;;     	     (- %1 %2))
;;     	 x1 x0)))

;; (defn update-guesses2 [f loss step x1 x0 e1]
;; (map #(- %1 (* step %2)) x1 (dloss-dx f loss x1 x0 e1)))

;; (defn sgd2
;; ([f loss step precision guesses examples]
;;    (sgd2 f loss step precision guesses
;; 	(repeat (count guesses) 0)
;; 	examples))
;; ([f loss step precision x1 x0 examples]
;;    (if (<= (euclidean-distance x1 x0) precision) x1
;;     (recur f loss step precision
;; 	   (update-guesses2 f loss step x1 x0
;; 			   (first examples))
;; 	   x1 (rest examples)))))

;; ;; (defn newton-step [X Bnext weights]
;; ;;   (let [mu (invlink eta)
;; ;;         z (plus (mult X Bnext) (mult (minus y mu) (dlink mu)))
;; ;;         W (diag weights)]
;; ;;         (mult 
;; ;;          (solve (mult (trans X) W X)) 
;; ;;          (trans X) W z)))

;; ;;http://en.wikipedia.org/wiki/Simulated_annealing
;; (defn sa
;; [loss s kmax acceptance examples]
;; (let [e (loss s)
;;       sbest s
;;       ebest e
;;       k 0]
;;  (if (not (and (< k kmax) (> e emax))) sbest
;;      (let [snew (first examples) ;;todo: not the right way to select neighbor
;; 	   enew (loss snew)
;; 	   [sbestnew ebestnew]
;; 	   (if (< enew ebest) [snew enew] [sbest ebest])
;; 	   [snext enext] (if (> (acceptance e enew (temp (/ k kmax)))
;; 				(rand)) [sbestnew ebestnew]
;; 				[sbest ebest])]
;;        (recur snext enext (+ k 1))))))


;;http://en.wikipedia.org/wiki/Regularization_(mathematics)