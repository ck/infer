(ns infer.sparse-reranking-test
  (:use infer.sparse-reranking clojure.test
	[infer.core :only [log-add]]
	[infer.measures :only [norm-sparse-vec]]))

(deftest mira-diff-vec-and-loss-test
  (is (=
       [{:bad -1 :good 1} 1]
       (mira-diff-vec-and-loss {}
	0 1	
	[0 1]
	[{:good 1} {:bad 1}]))))

(deftest mira-update-test
  (let [ws {:good -1 :bad 1}
	ls [0 1 1]
	datum [(norm-sparse-vec {:good 1})
	       (norm-sparse-vec {:good 1 :bad 1})
	       (norm-sparse-vec {:bad 1})]]
    (is (=
	 [1.5 {:good 1 :bad -1}]
	   (mira-update (constantly 2)  ws ls datum)))))

(deftest mira-iter-test
  (let [ws {:good -1 :bad 1}
	ls [0 1 2]
	data [(norm-sparse-vec {:good 1})
	      (norm-sparse-vec {:good 1 :bad 1})
	      (norm-sparse-vec {:bad 1})]]
    (is (= {:good 1 :bad -1}
	   (mira-iter {} (constantly [ls data]) [nil]  {:max-alpha 100})))))

(deftest train-mira-test
  (is (=
       {:good 1 :bad -1}
       (train-mira
	(constantly
	 [[0 1 2]
	   [(norm-sparse-vec {:good 1})
	    (norm-sparse-vec {:good 1 :bad 1})
	    (norm-sparse-vec {:bad 1})]])
	[nil]))))

(deftest get-posteriors-test
  (let [w {:p1 1.0 :p2 2.0}
	fvs [{:p1 1.0} {:p2 1.0}]]
    (is (= (log-add [1.0 2.0])
	   (first (get-posteriors w fvs))))))

(deftest rerank-grad-test
  (let [posts [0.33 0.67]
	datum [{:p1 1.0} {:p2 1.0}]]
    (is (= {:p2 -0.67 :p1 0.33}
	  (maxent-grad posts datum)))))

(deftest rerank-obj-term-test
  (let [w {:p1 1.0 :p2 3.0}
	d [{:p1 1} {:p2 1.0}]]
    (is (= [-2.1269280110429727 {:p2 -0.8807970779778823, :p1 0.11920292202211753}]
	 (maxent-obj-term w d)))))

(deftest rerank-obj-test
  (let [train [[{:p1 1.0} {:p2 1.0}] [{:p1 1.0} {:p3 1.0}]]
	preds [:p1 :p2 :p3]]
    (is (= [(* 2 (Math/log 2)) [-1.0 0.5 0.5]]
	     (maxent-obj train preds [0 0 0])))))

(deftest rerank-test
  (= {:p1 1.0 :p2 -0.5 :p3 -0.5}
     (train-maxent
      [[{:p1 1.0} {:p2 1.0}] [{:p1 1.0} {:p3 1.0}]])))


