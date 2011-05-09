(ns infer.probability-test
  (:use clojure.test
	infer.core
	infer.classification
	infer.probability))

(deftest counts-of-1
  (is (= {:a 1 :b 1}
	 ((classification-workflow
	   identity
	   #(classify-one-to-one
	     {:a (present-when (gt 5))
	      :b (present-when (lt 5))}
	     %)
	   (map-counter +)) [{:a 10 :b 5} {:a 4 :b 1}]))))

(deftest counts-by-fn
  (is  (=
	{:gt2 {:a 1 :b 0} :lt3 {:a 1 :b 2}}
	((classification-workflow
	  identity
	  #(classify-one-to-each
	    {:gt2 (present-when (gt 2))
	     :lt3 (present-when (lt 3))}
	    %)
	  (map-counter +))
	 [{:a 3 :b 2} {:a 2 :b 1}]))))

(deftest counts-each-cond-probs-test
  (is (= {:gt2  [{1 {0 1, 1 1}} {1 2}]}
   ((classification-workflow
    identity
      #(classify-one-to-all
       {:gt2 (P (present-when (tree-comp (gt 2) :a))
        | (present-when (tree-comp (lt 3) :b)))}
       %)
    (map-counter +cond-prob-tuples))
    [{:a 3 :b 2}
     {:a 2 :b 1}]))))

(deftest categorical-classification-test
  (is( =
       [{"HA" {"X" {1 1}}} {"HA" {"X" 1}}]
       ((P (present-when (constrain :z > 9))
	    | [:Carrier :Dep])
	{:Carrier "HA" :Dep "X" :z 10}))))

(deftest conditional-probability-counts
  (is( =
       [{"AA" {"Y" {0 1}, 
	       "Z" {1 1}},
	 "VA" {"Z" {0 2},
	       "Y" {1 1}},
	 "HA" {"X" {1 1}}}
	{"AA" {"Y" 1, "Z" 1},
	 "VA" {"Z" 2, "Y" 1},
	 "HA" {"X" 1}}]
       ((classification-workflow
	 identity
	 (P (present-when (constrain :z > 9))
	    | [:Carrier :Dep])
	 (vector-counter +cond-prob-tuples))
	[{:Carrier "HA" :Dep "X" :z 10}
	 {:Carrier "VA" :Dep "Y" :z 9.1}
	 {:Carrier "VA" :Dep "Z" :z 4}
	 {:Carrier "VA" :Dep "Z" :z 8.999}
	 {:frk "o" :Carrier "AA" :Dep "Z" :z 9.1}
	 {:f "foo" :Carrier "AA" :Dep "Y" :z 8.999}]))))

(deftest count-missing-test
  (is (= {:a 1}
   (count-missing
    #{:a :b}
    {:b 1 :c 2}))))

(deftest combinatorial-merge-test
  (is (=
       {1 {1 12
     2 6
     3 3
     4 3}
  2 {1 20
     2 10
     3 5
     4 5}}
       (comb-merge * {1 3
          2 5}
       {1 4
        2 2
        3 1
        4 1}))))

(deftest summations
  (is (= 3
   (summate {1 {2 3}})))
  (is (= {1 16, 2 27}
   (summate-level   {1 {1 {1 2, 2 3}
            2 {1 5, 2 6}}
         2 {1 {3 6, 4 7}
            2 {5 6, 7 8}}}))))

(deftest extract-marginals-from-joint
  (let [py {1 24, 2 40}
  pz  {1 32, 2 16, 3 8, 4 8}
  independent-joint
  {1 {1 12
      2 6
      3 3
      4 3}
   2 {1 20
      2 10
      3 5
      4 5}}]
    (is (= [py pz]
     (marginals independent-joint))))
  (let [independent-joint
  {1 {1 {1 2, 2 3}
      2 {1 5, 2 6}}
   2 {1 {3 6, 4 7}
      2 {5 6, 7 8}}}]
    (is (= [{1 16, 2 27} {1 18, 2 25} {5 6, 7 8, 4 7, 3 6, 1 7, 2 9}]
     (marginals independent-joint)))))

;;TODO: hacked in here for the time being, should be killed and removed fromtests or supported as first class temporal concept.
(defn present [k] #(k (second %)))
(defn previous [k] #(k (first %)))

(deftest greater-than-constraint
  (is ((constrain :a > 5) {:a 6}))
  (is (not ((constrain :a > 5) {:a 4}))))

(deftest simple-presense
  (is (= 1 ((present-when (gt 5)) 6))))

(deftest present-when-with-keys-as-args
  (is (= 1
   ((present-when
     > :a :b)
    {:b 20 :a 21}))))

(deftest present-when-with-explicit-number-of-args
  (is (= 1
   ((present-when
     #(> %1 %2) :a :b)
    {:b 20 :a 21}))))

(deftest nil-coll-test
  (is (= true
       (nil-coll? nil)))
  (is (= false
       (nil-coll? []))))

(deftest conditional-prob-notation
  (is (=  [{1 {0 1}} {1 1}]
   ((P (present-when (gt 5)) | (present-when (lt 10))) 4)))
  (is (= [{1 {1 1}} {1 1}]
   ((P (present-when (gt 5)) | (present-when (lt 10))) 6))))

(deftest present-observation
  (is (= true
   ((constrain (present :arrdelay15) = 1)
    [{}  {:tailnum 5 :depdelay15 1 :arrdelay15 1}]))))

(deftest previous-observation
  (is (= true
   ((constrain (previous :arrdelay15) = 1)
    [{:tailnum 5 :depdelay15 1 :arrdelay15 1} {}]))))

(deftest tail-number-conditional-count
  (is (= [{1 {1 1}} {1 1}]
   ((P (present-when (constrain (present :arrdelay15) = 1)) |
       (present-when (constrain (previous :depdelay15) = 1)))
    [{:tailnum 5 :depdelay15 1 :arrdelay15 0}
     {:tailnum 5 :depdelay15 1 :arrdelay15 1}]))))

(deftest classify-in-range
  (is (= 5 (range-classifier (range 1 10) 5.001)))
  (is (= 0 (range-classifier (range 1 10) 0.001)))
  (is (= 1 (range-classifier  (range (* 60 -3) (* 60 10) 60) -175)))
  (is (= 9 (range-classifier (range 1 10) 10.01)))
  (is (= 4 (range-classifier (range 1 10) 5))))

(deftest classify-in-15min-intervals
  (is (= 0 (range-classifier (range 0 61 15) 0)))
  (is (= 1 (range-classifier (range 0 61 15) 0.10)))
  (is (= 2 (range-classifier (range 0 61 15) 15.01)))
  (is (= 3 (range-classifier (range 0 61 15) 30.01)))
  (is (= 4 (range-classifier (range 0 61 15) 60)))
  (is (= 5 (range-classifier (range 0 61 15) 61))))

(deftest prange-test
  (is (= [{4 {3 1}} {4 1}]
   ((P (bucket :foo (range 0 10 1)) |
       (bucket :bar (range 0 10 1)))
    {:foo 3 :bar 4}))))

(deftest prange-temporal
  (is (= [{4 {6 1}} {4 1}]
   ((P (bucket (present :foo) (range 0 10 1)) |
       (bucket (previous :bar) (range 0 10 1)))
    [{:foo 3 :bar 4} {:foo 6 :bar 5}]))))

(deftest prange-temporal-reducer-empty
  (is (= [{3 {5 1}} {3 1}]
   ((classification-workflow
     (rolling-windows 2)
     (P (bucket (present :foo) (range 1 11 1)) |
        (bucket (previous :bar) (range 1 11 1)))
     (vector-counter +cond-prob-tuples))
     [{:foo 3 :bar 4}
     {:foo 6 :bar 5}]))))

(deftest prange-temporal-reducer-with-1s
  (is (= [{0 {0 1}} {0 1}]
  ((classification-workflow
     (rolling-windows 2)
     (P (bucket (present :foo) (range 1 11 1)) |
        (bucket (previous :bar) (range 1 11 1)))
     (vector-counter +cond-prob-tuples))
     [{:foo 3 :bar 1}
      {:foo 1 :bar 5}]))))

(deftest range-persistence-prob-test
  (is (= [{6 {1 1}, 5 {3 1}, 4 {6 1}} {6 1, 5 1, 4 1}]
   ((classification-workflow
     (rolling-windows 2)
     (P (bucket (present :foo) (range 0 10 1)) |
        (bucket (previous :bar) (range 0 10 1)))
     (vector-counter
     +cond-prob-tuples))
     [{:foo 3 :bar 4}
      {:foo 6 :bar 5}
      {:foo 3 :bar 6}
      {:foo 1 :bar 2}]))))

(deftest tail-number-persistence-prob
  (is (= [{0 {0 1}, 1 {1 2}} {0 1, 1 2}]
   ((classification-workflow
     (rolling-windows 2)
    (P (present-when (constrain (present :arrdelay15) = 1)) |
       (present-when (constrain (previous :depdelay15) = 1)))
    (vector-counter +cond-prob-tuples))
    [{:tailnum 5 :depdelay15 1 :arrdelay15 0}
     {:tailnum 5 :depdelay15 0 :arrdelay15 1}
     {:tailnum 5 :depdelay15 1 :arrdelay15 0}
     {:tailnum 5 :depdelay15 0 :arrdelay15 1}]))))

(deftest conditional-probability-in-ranges
  (is( =
  [{1 {0 2}, 2 {0 1, 1 2}, 0 {1 1}} {1 2, 2 3, 0 1}]
       ((classification-workflow
   identity
   (P (present-when (constrain :z > 9))
      | (discretizing-classifiers {:Carrier (range 1 5 1)}))
   (vector-counter +cond-prob-tuples))
       [{:Carrier 1 :Dep "X" :z 10}
        {:Carrier 3 :Dep "Y" :z 9.1}
        {:Carrier 2 :Dep "Z" :z 4}
        {:Carrier 3 :Dep "Z" :z 8.999}
        {:frk "o" :Carrier 3 :Dep "Z" :z 9.1}
        {:f "foo" :Carrier 2 :Dep "Y" :z 8.999}]))))

(deftest realtime-flightstats-persistence-prob-test
  (is (= [{6 {1 1}, 5 {3 1}, 4 {6 1}} {6 1, 5 1, 4 1}]
   ((classification-workflow
     (rolling-windows 2)
     (P (bucket (present :foo) (range 0 10 1)) |
        (bucket (previous :bar) (range 0 10 1)))
     (vector-counter +cond-prob-tuples))
     [{:foo 3 :bar 4}
      {:foo 6 :bar 5}
      {:foo 3 :bar 6}
      {:foo 1 :bar 2}]))))


(deftest funky-p-test
  (is (= 12
   (P 3 #(apply + %1 %2) 2 3 4)))
  (is (= 12
   (P 3 #(apply + %1 %2) 9))))

(deftest multi-node-tree
  (is (= [{6 {10 {1 1}}, 5 {10 {3 1}}, 4 {10 {6 1}}} {6 {10 1}, 5 {10 1}, 4 {10 1}}]
   ((classification-workflow
     (rolling-windows 2)
     (P (bucket (present :foo) (range 0 10 1)) |
        (bucket (previous :bar) (range 0 10 1))
        (bucket (previous :baz) (range 0 10 1)))
     (vector-counter +cond-prob-tuples))
     [{:foo 3 :bar 4 :baz 15}
      {:foo 6 :bar 5 :baz 15}
      {:foo 3 :bar 6 :baz 15}
      {:foo 1 :bar 2 :baz 15}]))))

(deftest missing-test
  (is (= true
   (missing? {:a nil})))
  (is (= true
   (missing? [1 4 :a nil])))
  (is (= true
   (missing? {:a [:foo 1 nil]}))))

(deftest missing-test
  (is (= true
   (missing? {:a nil})))
  (is (= true
   (missing? [1 4 :a nil])))
  (is (= false
   (missing? {:a [:foo 1 nil]})))
  (is (= true
   (missing? {:a 1 :b nil}))))

(deftest bucketing
 (is (= 6
   ((bucket
    #(* 2 %) :foo (range 0 10 1))
    {:foo 3 :bar 4})))
 (is (= 7
   ((bucket
    (fn [[x y]] (+ x y))
    #(vector (:foo %) (:bar %))
    (range 0 10 1))
    {:foo 3 :bar 4})))
 (is (= :missing
   ((bucket
    (fn [[x y]] (+ x y))
    :foo
    (range 0 10 1))
    {:foo nil}))))

(deftest tree-comp-with-p
  (is (= [{1 {0 1}} {1 1}]
	 ((P (present-when (tree-comp > :a 5)) | (present-when (tree-comp < :b 10))) {:a 4 :b 5})))
  (is (= [{1 {1 1}} {1 1}]
	 ((P (present-when (tree-comp > :a 5)) | (present-when (tree-comp < :b 10))) {:a 6 :b 6}))))

