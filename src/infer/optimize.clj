(ns infer.optimize
  ^{:doc "Standard functions for numerical optimziation. Only very marginally tuned
   for performance. Lots of optional configurability. Defaults are optimized for 
   either gradient descent or quasi-Newton methods, which is what you'll end up doing
   for many batch trained ML algorithms. 
         
   NOTE: This ns assumes you're trying to always minimize, rather than maximize a function. 
   In general the functions we optimize take a double seq x and return [value grad]
   pair, where val = f(x) and grad = df/dx(x) 
   
   Different from infer.learning, which mixes optimization with actual objectives, which is approriate
   for the kind of problems there. This ns is for arbitrary function optimization. 
   You give me a function with a value and gradient, I'll optimize it!"   
   :author "Aria Haghighi <me@aria42.com>"}
  (:use [clojure.contrib.def :only [defvar]]
        [infer.measures :only [within dot-product]]))
    
;; Global Variables/Options  
(defvar *step-size-underflow* 1.0e-20 "When to bail on line search")
(defvar *eps* 1.0e-4 "Tolerance parameter for convergence")

(defn- make-affine-fn 
  "for number seqs x,y, return f: alpha -> x + alpha* y"
  [x y]
  (fn [alpha] (map #(+ %1 (* alpha %2)) x y)))

(defn- make-step-fn 
  "for a function f, returns the function 
   step-size -> f(x0 + step-size * step-dir)
   
   This is the function which line search would optimzie. 
   
   f: arbitrary function takes seq of numbers as input
   x0: initial point for f
   step-dir: direction for f"  
  [f x0 step-dir]
  (comp f (make-affine-fn x0 step-dir)))                          

(defn remember-last 
  "remember last input and output for function f. Assumes
  f only takes one argument useful for caching recent optimization
  fn queries"
  [f]
  (let [last-arg (atom nil) last-val (atom nil)]
    (fn [x]
      (if (= @last-arg x)
        @last-val
        (let [val (f x)]
          (swap! last-arg (constantly x))
          (swap! last-val (constantly val))
          val)))))

(defn backtracking-line-search 
  "search for step-size in step-dir to satisfy Wolfe Conditions:
     f(x0 + step-size*step-dir) <= f(x0) + suff-descrease * step-size * grad-f(x_0)' delta
   returns satisfying step-size
   
   f: x -> [f(x) grad-f(x)]
   x0: initial point (use this to get dimension of f). should be seqable
   step-dir: vector of same dimensions as x0. should be seqable
   
   Options are...
   suff-descrease: Wolfe condition constant (default to 1.0e-4)
   step-size-multiplier: How much to decrease step-size by each iter (default to 0.1)
      
   TODO: Add curvature lower-bound"   
  [f x0 step-dir &
    {:as opts
     :keys [suff-decrease, step-size-multiplier] 
     :or {suff-decrease 0.001 step-size-multiplier 0.1}}]
    (let [[val grad]  (f x0)
          step-fn (make-step-fn f x0 step-dir)
          directional-deriv (dot-product step-dir grad)
          target-val (fn [step-size] (+ val (* suff-decrease step-size directional-deriv)))]
      (loop [step-size 1.0]        
        (cond
          (< step-size *step-size-underflow*)
            (RuntimeException. "Line search: Stepsize underflow. Probably a gradient computation error")
          (<= (-> step-size step-fn first) (target-val step-size))  
            step-size            
          :default (recur (* step-size-multiplier step-size))))))
          
(defprotocol QuasiNewtonApproximation
  (inv-hessian-times [this z] "Implicitly multiply H^-1 z for quasi-newton search direction")
  (update-approx [this x-new x-old grad-new grad-old] 
    "Update approximation after new x and grad points. Should return a new QuasiNewtonApproximation"))
  
(defrecord LBFGSApproximation [max-history-size x-deltas grad-deltas]

  QuasiNewtonApproximation
  ; See http://en.wikipedia.org/wiki/L-BFGS  
  (inv-hessian-times [this z]
    (let [alphas 
            (map 
              (fn [x-delta grad-delta] 
                (let [curvature (dot-product x-delta grad-delta)]
                  (when (< curvature 0)
                    (throw (RuntimeException. 
                      (str "Non-positive curvature: " x-delta " " grad-delta))))
                  (* (/ (dot-product x-delta z) curvature))))
               x-deltas grad-deltas)
          left 
            (loop [res z alphas alphas grad-deltas grad-deltas]
              (if (or (empty? alphas) (empty? grad-deltas))
                res
                (let [alpha (first alphas) grad-delta (first grad-deltas)]
                  (recur (map #(- %1 (* alpha %2)) res grad-delta)
                         (rest alphas)
                         (rest grad-deltas)))))]
      (loop [res left alphas alphas x-deltas x-deltas grad-deltas grad-deltas]
        (if (some empty? [alphas x-deltas grad-deltas])
          res
          (let [alpha (first alphas)
                x-delta (first x-deltas)
                grad-delta (first grad-deltas)
                rho (/ 1.0 (dot-product x-delta grad-delta))
                b (* rho (dot-product grad-delta res))]
            (recur ((make-affine-fn res x-delta) (- alpha b))
                   (rest alphas)
                   (rest x-deltas)
                   (rest grad-deltas)))))))

; (reduce (fn [res [alpha grad-delta]]
;                 (map #(- %1 (* alpha %2)) res grad-delta))
;              z (map vector alphas grad-deltas))]
; (reduce (fn [res [alpha x-delta grad-delta]]
;           (let [rho (/ 1.0 (dot-product x-delta grad-delta))
;                 b (* rho (dot-product grad-delta res))]
;             ((make-affine-fn res x-delta) (- alpha b))))
;         left
;         (map vector alphas x-deltas grad-deltas))))

                        
  (update-approx [this x-new x-old grad-new grad-old]
    (let [x-delta (map - x-new x-old)
          grad-delta (map - grad-new grad-old)]
      (LBFGSApproximation.
        max-history-size
        (take max-history-size (conj x-deltas x-delta))
        (take max-history-size (conj grad-deltas grad-delta))))))
  
(defn- new-lbfgs-approx [max-history-size]
  (LBFGSApproximation. max-history-size '() '()))  
            
         
(defn- quasi-newton-iter
  "A single iteration of quasi-newton optimization. Takes same options as quasi-newton-optimize.
   Returns new estimate of arg min f
  
  f: x => [f(x) grad-f(x)]
  x0: current estimate
  qn-approx: Implements QuasiNewtonApproximation
  for efficiency, you want to implicitly multiply inv-hessian approximation 
  with target vector using quasi-newton approximation. "
  [f x0 qn-approx opts]
  (let [grad  (second (f x0))        
        step-dir (map - (inv-hessian-times qn-approx grad))
        step-fn (make-step-fn f x0 step-dir)
        step-size (apply backtracking-line-search f x0 step-dir (flatten opts))]
    ((make-affine-fn x0 step-dir) step-size)))
    
    
(comment
  (def f (fn [[x]] [(- (* x x) 1) [(* 2 x)]]))
  (quasi-newton-iter f [10] (new-lbfgs-approx 1) {})
  (quasi-newton-optimize f [10] (new-lbfgs-approx 1))
  flatten)    
                      
(defn quasi-newton-optimize 
  "Does quasi-newton optimization. Pass in QuasiNewtonApproximation
   to do either exact newton optimization or LBFGS. Parameters:
   
   qn-approx: Implements QuasiNewtonApproximation (should use (new-lbfgs-approx))
   f: function to optimize
   x0: initial point
      
   Has several options you don't need to worry about
   max-iters: How many iterations before we bail (default 50)
   step-size-multiplier: Step-size-multiplier for inner line search
   init-step-size-multiplier: Step-size multiplier for first iter (default 0.5). 
   For many convex functions, the first line search should be more careful to get
   scale of problems. Definitely true for conditional likelihood objectives. "
  [f x0 qn-approx &
   {:as opts
    :keys [history-size, max-iters, init-step-size-multiplier, step-size-multiplier, print-progress]
    :or {history-size 9, max-iters 50, init-step-size-multiplier 0.5, step-size-multiplier 0.1}}]  
  (loop [iter 0 x x0 qn-approx qn-approx]
    (let [[val grad] (f x)          
          new-x (quasi-newton-iter f x qn-approx
                  (if (zero? iter)
                    (assoc opts :step-size-multiplier init-step-size-multiplier) 
                    opts))
          [new-val new-grad] (f new-x)
          converged? (within *eps* val new-val)]
      (when print-progress (println (format "At start of iter %d, value is %.5f\n" iter val)))    
      (if (or converged? (= iter max-iters))
        x        
        (recur (inc iter) new-x (update-approx qn-approx new-x x new-grad grad))))))
        
        
(defn lbfgs-optimize
  "Run LBFGS Optimization. Convenience wrapper for quasi-newton-optimize with LBFGS as the QuasiNewton
   approximation. You can pass in any options that quasi-newton-optimize uses, but also there is a 
   :max-history-size (default 9) option specific to LBFGS"
  [f x0 &
    {:as opts
     :keys [max-history-size]  
     :or {max-history-size 9}}]
  (apply quasi-newton-optimize f x0 (new-lbfgs-approx max-history-size) (-> opts seq flatten)))        