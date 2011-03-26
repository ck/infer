(ns infer.sparse-classification-test
  (:use clojure.test
	infer.core
        infer.sparse-classification))

(deftest l2-test
  (let [obj-fn (fn [[x]] [(* x x) [(* 2 x)]])]
    (= [1.5 [3.0]]
       (with-l2-regularization 1.0 obj-fn [1.0]))))

(deftest learn-logistic-regression-test
  (let [data [[:cat {:small 1 :fuzzy 1 :meows 1}]
              [:dog {:large 1 :fuzzy 1 :barks 1}]
              [:cat {:large 1 :meows 1}]]
        ; Learn the model, serialize to object, read object
        model (learn-logistic-regression data)
        model-serialized (serialize-obj model)
        model-read (load-classifier model-serialized)
        posterior-predict (partial label-posteriors model-read)]
    (is (> (-> {:small 1 :meows 1} posterior-predict :cat) 0.5))
        (> (-> {:fuzzy 1 :large 1} posterior-predict :dog) 0.5)))     

(deftest rerank-post-test
  (let [w {:p1 1.0 :p2 2.0}
	fvs [{:p1 1.0} {:p2 1.0}]]
    (rerank-post w fvs)
    (is (= (log-add [1.0 2.0])
	   (first (rerank-post w fvs))))))

(deftest rerank-grad-test
  (let [posts [0.33 0.67]
	datum [{:p1 1.0} {:p2 1.0}]]
    (is (= {:p2 -0.67 :p1 0.33}
	  (rerank-grad posts datum)))))

(deftest rerank-obj-term-test
  (let [w {:p1 1.0 :p2 3.0}
	d [{:p1 1} {:p2 1.0}]]
    (is (= [-2.1269280110429727 {:p2 -0.8807970779778823, :p1 0.11920292202211753}]
	 (rerank-obj-term w d)))))

(deftest rerank-obj-test
  (let [train [[{:p1 1.0} {:p2 1.0}] [{:p1 1.0} {:p3 1.0}]]
	preds [:p1 :p2 :p3]]
    (is (= [(* 2 (Math/log 2)) [-1.0 0.5 0.5]]
	     (reranker-obj train preds [0 0 0])))))

(deftest rerank-test
  (= {:p1 1.0 :p2 -0.5 :p3 -0.5}
     (train-reranker
      [[{:p1 1.0} {:p2 1.0}] [{:p1 1.0} {:p3 1.0}]])))


(deftest mira-test
  (let [mira (-> (new-mira {[:cat :dog] 1.0 [:dog :cat] 1.0})
                 (update-model :cat {:small 1 :fuzzy 1 :meows 1})
                 (update-model :dog {:large 1 :fuzzy 1 :barks 1})
                 (update-model :cat {:large 1 :meows 1}))]
    (is (predict-label mira {:small 1 :meows 1}) :cat)))
