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



(deftest mira-test
  (let [mira (-> (new-mira {[:cat :dog] 1.0 [:dog :cat] 1.0})
                 (update-model :cat {:small 1 :fuzzy 1 :meows 1})
                 (update-model :dog {:large 1 :fuzzy 1 :barks 1})
                 (update-model :cat {:large 1 :meows 1}))]
    (is (predict-label mira {:small 1 :meows 1}) :cat)))
