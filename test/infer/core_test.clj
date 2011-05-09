(ns infer.core-test
  (:use clojure.test)
  (:use infer.core))

(deftest get-all-keys
  (is (= #{1 2 3 4}
	 (all-keys {1 {2 0, 3 0}, 2 {4 1}})))
  (is (= #{1 2 3}
	 (all-keys {1 6, 2 5, 3 4})))
  (is (= #{1 2 3}
	 (all-keys (vals {1 {1 6, 2 5, 3 4}})))))

(deftest bottom-level-test
(is (= true
 (bottom-level? {:a 1})))
(is (= false
 (bottom-level? {:a {:b 1}}))))

(deftest levels-deep-test
  (is (= 1
	 (levels-deep {0 1})))
  (is (= 2
	 (levels-deep {1 {0 1}})))
  (is (= 0
	 (levels-deep {})))
  (is (= 0
	 (levels-deep 1)))
  (is (= 0
	 (levels-deep nil)))
  (is (= 4
	 (levels-deep {1 {0 {0 1, 1 {0 1}}}}))))

(deftest flatten-map-test
  (is (= {":a:b:c" 1}
	 (flatten-with str {:a {:b {:c 1}}}))))

(deftest tree-comp-basic
  (is (= 10
	 ((tree-comp * #(- % 3) identity) 5))))

(deftest tree-comp-nil-short-circuit
  (is (nil? ((tree-comp + :a :b) {:a nil :b nil})))
  (is (nil? ((tree-comp + :a :b) {:a 1   :b nil})))
  (is (nil? ((tree-comp + :a :b) {})))
  (is (nil? ((tree-comp + first second) [nil nil]))))

(deftest tree-comp-apply-f-to-map-args
  (is (= 3 ((tree-comp + :a) {:a 3})))
  (is (= 3 ((tree-comp + :a :b) {:a 1 :b 2})))
  (is (= 3 ((tree-comp + :a 2) {:a 1 :b 2})))
  (is (= -1
    ((tree-comp #(- %1 %2) :a :b) {:a 1 :b 2})))
  (is (= -1
    (((fn [a & b] (apply tree-comp a b)) #(- %1 %2) :a :b) {:a 1 :b 2}))))

(deftest tree-comp-nested
  (is (= 3
   ((tree-comp
      (tree-comp
         #(+ %1 %2)
         :a :b)
      :c)
      {:c {:a 1 :b 2}}))))
