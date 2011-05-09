(ns infer.matrix
  (:use clojure-csv.core
	infer.matrix
	clojure.set)
  (:import java.util.Random
	   [org.ujmp.core Matrix
	    MatrixFactory
	    Ops]
	   [org.ujmp.core.matrix Matrix2D]
	   [org.ujmp.colt
	    ColtSparseDoubleMatrix2D]
	   [org.ujmp.parallelcolt
	    ParallelColtSparseDoubleMatrix2D]
	   [org.ujmp.core.doublematrix
	    DoubleMatrix DenseDoubleMatrix2D
	    DoubleMatrix2D SparseDoubleMatrix2D]
	   org.ujmp.core.calculation.Calculation$Ret
	   org.ujmp.core.doublematrix.calculation.general.decomposition.Chol))

(defn leave-out [js ys]
  (difference (into #{} ys) (into #{} js)))

(defn ensure-vecs [xs]
  (let [to-vec #(if (vector? %) % (vec %))]
    (to-vec (map to-vec xs))))

(defn doubles-2d [xs]
  (let [vecs (ensure-vecs xs)
	#^"[[D" arr
	(make-array Double/TYPE (count vecs) (count (first vecs)))]
    (dotimes [idx (count vecs)]
	   (aset arr (int idx)
		 #^doubles (double-array (nth vecs idx))))
    arr))

(defn with-intercept [xs]
  (if (not (coll? (first xs))) ;;1 column
    (map #(vector 1 %) xs)
    (map #(vec (cons 1 %)) xs)))

(defn matrix [xs]
  (cond (not (coll? xs)) ;;already a matrix
	xs
	:else
	(MatrixFactory/importFromArray #^"[[D" (doubles-2d xs))))

(defn- sparse-matrix* [xs mk-matrix]
  (let [n-rows (count xs)
	cols (reduce (fn [acc row]
		       (union acc (into #{} (keys row))))
		     #{}
		     xs)
	#^DoubleMatrix2D m (mk-matrix (long-array [n-rows (+ (apply max cols) 1)]))
	row-indices (range 0 (count xs))]
    (dorun
     (map (fn [row r]
	    (dorun (map (fn [[c v]]
			  (.setDouble m v (long-array [r c])))
			row)))
	  xs
	  row-indices))
    m))

(defn sparse-matrix [xs]
 (sparse-matrix* xs (fn [#^"[J" x] (MatrixFactory/sparse x))))

(defn sparse-colt-matrix [xs]
 (sparse-matrix* xs (fn [#^"[J" x] (ColtSparseDoubleMatrix2D. x))))

(defn sparse-pcolt-matrix [xs]
 (sparse-matrix* xs (fn [#^"[J" x] (ParallelColtSparseDoubleMatrix2D. x))))

;; (defn sparse-mahout-matrix [xs]
;;   (let [n-rows (count xs)
;; 	cols (reduce (fn [acc row]
;; 		       (union acc (into #{} (keys row))))
;; 		     #{}
;; 		     xs)
;; 	m (SparseMatrix. (long-array [n-rows (+ (apply max cols) 1)]))
;; 	row-indices (range 0 (count xs))]
;;     (dorun
;;      (map (fn [row r]
;; 	    (dorun (map (fn [[c v]]
;; 			  (.setQuick m r c v)
;; 			row)))
;; 	  xs
;; 	  row-indices))
;;     m)))

(defn from-sparse-matrix [#^DoubleMatrix2D m]
  (map (fn [coord]
	 (conj (into [] (map int coord)) (.getDouble m coord)))
       (.availableCoordinates m)))

(defn from-sparse-2d-matrix [m]
  (let [map-row (fn [[r row]] (into {} (map (fn [[_ b c]] [b c]) row)))]
    (map map-row
	 (group-by first (from-sparse-matrix m)))))

(defn column-matrix [ys]
  (matrix (map vector ys)))

(defn from-matrix [#^DoubleMatrix2D A]
  (map #(into [] %)
       (.toDoubleArray A)))

(defn from-column-matrix [#^DoubleMatrix2D X]
  (flatten (map #(into [] %)
		(.toDoubleArray X))))

(defn fill [v r c]
  (MatrixFactory/fill v (long-array [r c])))

(defn I
"identity matrix"
[& dimensions] (MatrixFactory/eye (long-array dimensions)))

; (defn zero-matrix
;   "Creates a matrix of zeros of dimension r c."
;   [r c]
;   (MatrixFactory/zeros (long-array [r c])))

(defn rand-elems
  ([n]
     (let [ra (Random.)]
       (repeatedly n #(.nextDouble ra))))
  ([r c] 
     (repeatedly r #(rand-elems c))))

;;TODO: replace random matrix with.
;; // create a matrix filled with random numbers (Gaussian distribution):
;; Matrix m5 = MatrixFactory.randn(10, 3);
 
;; // create a matrix filled with random numbers (uniform distribution):
;; Matrix m6 = MatrixFactory.rand(5, 5);

(defn fill-rand [r c]
  (matrix (rand-elems r c)))

(defn to-diag [xs]
  (let [n (count xs)
	is (range 0 (+ 1 n))
	zeros (vec (repeat n 0))]
    (map (fn [x i]
	   (assoc zeros i x))
	 xs is)))

(defn column-count [#^DenseDoubleMatrix2D A]
  (.getColumnCount A))

(defn row-count [#^DenseDoubleMatrix2D A]
  (.getRowCount A))

(defn count-elements [M]
  (* (row-count M)
     (column-count M)))

(defn get-at [#^DoubleMatrix2D m r c]
  (.getDouble m (int r) (int c)))

(defn set-at [#^DoubleMatrix2D m v r c]
 (.setDouble m (double v) (int r) (int c)))

(defn copy-matrix [#^DoubleMatrix2D m]
 (.copy m))

(defn inc-at
  ([m r c] (inc-at m 1 r c))
  ([m by r c]
     (set-at m
	     (+ (get-at m r c) by)
	     r c)
     m))

;;TODO: abstract general pattern for calling these operations on the underlying matrix object
(defn times
  "Performs matrix multiplication of A, an n x k matrix, and B, a k x l matrix,
  and outputs an n x l matrix.  If there are more matrices Ms, continues to
  multiply them all from left to right in the usual fashion."
  ([#^DenseDoubleMatrix2D A B]
  (if (isa? (class B) DenseDoubleMatrix2D)
  (.mtimes A #^DenseDoubleMatrix2D B)
  (.mtimes A #^Double (double B))))
  ([#^DenseDoubleMatrix2D A B & Ms]
     (let [AB (times A B)]
       (if Ms
	 (recur AB (first Ms) (next Ms))
	 AB))))

(defn divide
  ([#^DenseDoubleMatrix2D A B]
  (if (isa? (class B) DenseDoubleMatrix2D)
  (.divide A #^DenseDoubleMatrix2D B)
  (.divide A #^Double (double B))))
  ([#^DenseDoubleMatrix2D A B & Ms]
     (let [AB (divide A B)]
       (if Ms
	 (recur AB (first Ms) (next Ms))
	 AB))))

(defn plus
  "Adds matrices A in B in the usual way, assuming they are of the same size.
  If additional matrices Ms are supplied, adds them all from left to right."
  ([#^DenseDoubleMatrix2D A B]
  (if (isa? (class B) DenseDoubleMatrix2D)
  (.plus A #^DenseDoubleMatrix2D B)
  (.plus A #^Double (double B))))
  ([#^DenseDoubleMatrix2D A B & Ms]
     (let [AB (plus A B)]
       (if Ms
	 (recur AB (first Ms) (next Ms))
	 AB))))

(defn minus
    "Subtracts matrices A in B in the usual way, assuming they are of the same size.
  If additional matrices Ms are supplied, subtracts them all from left to right."
  ([#^DenseDoubleMatrix2D A B]
  (if (isa? (class B) DenseDoubleMatrix2D)
  (.minus A #^DenseDoubleMatrix2D B)
  (.minus A #^Double (double B))))
  ([#^DenseDoubleMatrix2D A B & Ms]
     (let [AB (minus A B)]
       (if Ms
	 (recur AB (first Ms) (next Ms))
	 AB))))

(defn elementwise-plus [e M]
(plus (fill e (row-count M) (column-count M))
       M))

(defn elementwise-minus [e M]
(minus (fill e (row-count M) (column-count M))
       M))

(defn trans [#^DenseDoubleMatrix2D A]
  (.transpose A))

(def rows from-matrix)

(defn columns [A]
  (from-matrix (trans A)))

(defn svd [#^DenseDoubleMatrix2D A]
  (.svd A))

(defn qr [#^DenseDoubleMatrix2D A]
  (.qr A))

(defn chol [#^DenseDoubleMatrix2D A]
  (.chol A))

(defn solve [#^DenseDoubleMatrix2D A #^DenseDoubleMatrix2D B]
  (.solve A B))

(defn inv [#^DenseDoubleMatrix2D A]
  (.inv A))

(def link-to-matrix Calculation$Ret/LINK)
(def new-matrix Calculation$Ret/NEW)
(def orig-matrix Calculation$Ret/ORIG)

(defn delete-rows
  [#^DoubleMatrix2D A rows]
  (let [#^Calculation$Ret m new-matrix]
    (.deleteRows A m (long-array rows))))

(defn delete-columns 
  [#^DoubleMatrix2D A columns]
  (let [#^Calculation$Ret m new-matrix]
    (.deleteColumns A m (long-array columns))))

(defn select-columns
  [#^DoubleMatrix2D A columns]
  (let [#^Calculation$Ret m new-matrix]
    (.selectColumns A m (long-array columns))))

(defn select-rows
  [#^DoubleMatrix2D A rows]
  (let [#^Calculation$Ret m new-matrix]
    (.selectRows A m (long-array rows))))

(defn column-concat 
  [& Ms] (MatrixFactory/horCat #^"[Lorg.ujmp.core.doublematrix.DoubleMatrix2D;"
			       (into-array #^Matrix Ms)))

(defn row-concat 
  [& Ms] (MatrixFactory/vertCat #^"[Lorg.ujmp.core.doublematrix.DoubleMatrix2D;"
				(into-array #^Matrix Ms)))

(defn each-row 
	"Returns a lazy sequence of row vectors, corresponding to the rows of matrix A."
	[#^DenseDoubleMatrix2D A]
	(map #(select-rows A [%]) (range 0 (row-count A))))

(defn each-column
	"Returns a lazy sequence of column vectors, corresponding to the columns of matrix A."
	[#^DenseDoubleMatrix2D A]
	(map #(select-columns A [%]) (range 0 (column-count A))))

(defn convolve 
  ^{:doc "Perform matrix convulotion on Dense double Matrix M
   against kernel matrix K. Essentially, for each element (i,j)
   of M, weigh neighboring elements of (i,j) according
   to K centered on (i,j) and return new matrix 
   where (i,j) is the average of surrounding points
   weighed by K. 
   
   The boundary counditions just truncate the matrix K
   so it's as though K is padded with 0's if applied to a 
   point on the edge.
   
   Returns new matrix, no mutation of input matrices."
   :author "Aria Haghighi <me@aria42.com>"}
  [#^DenseDoubleMatrix2D M #^DenseDoubleMatrix2D K]
  (let [num-rows (int (row-count M))
        num-cols (int (column-count M))
        kernel-row-rad (-> K row-count (/ 2) int)
        kernel-column-rad (-> K column-count (/ 2) int)        
        compute-kernel 
          (fn [x y]
            (let [x0 (Math/max (int (- x kernel-row-rad)) (int 0))
                  x1 (Math/min (int (inc (+ x kernel-row-rad))) (int num-rows))
                  y0 (Math/max (int (- y kernel-column-rad)) (int 0))
                  y1 (Math/min (int (inc (+ y kernel-column-rad))) (int num-cols))
                  kx0 (if (< x kernel-row-rad) x 0)
                  kx1 (+ kx0 (- x1 x0))
                  ky0 (if (< y kernel-column-rad) y 0)
                  ky1 (+ ky0 (- y1 y0))
                  M-sub (.subMatrix M link-to-matrix x0 y0 (dec x1) (dec y1))
                  K-sub (.subMatrix K link-to-matrix kx0 ky0 (dec kx1) (dec ky1))]
              (/ (.getValueSum (.times M-sub K-sub))
                 (.getValueSum K-sub))))
         R (fill 0.0 num-rows num-cols)]
    (doseq [x (range 0 num-rows) y (range 0 num-cols)]
      (set-at R (compute-kernel x y) x y))
    R))

(defn csv->matrix [path]
  (let [strings (parse-csv (slurp path))]
    (matrix (for [row strings
		  :when (not (some #(= "" %) row))]
		   (map #(Float/parseFloat  %) row)))))