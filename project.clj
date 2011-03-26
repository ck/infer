(defproject infer "1.0-SNAPSHOT"
  :description "inference and machine learning for clojure"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
		 [clojure-csv/clojure-csv "1.1.0"]
		 [org.apache.commons/commons-math "2.0"]
                 [ujmp-complete "0.2.4"]
		 [org.apache.mahout/mahout-core "0.3"]
		 [clj-sys/plumbing "0.1.3-SNAPSHOT"]
		 [colt/colt "1.2.0"]
		 [incanter/parallelcolt "0.9.4"]]
  :dev-dependencies [[org.clojars.mmcgrana/lein-javac "1.2.1"]
		     [swank-clojure "1.2.0"]
                     [lein-clojars "0.5.0"]]
  :java-source-path "src/jvm")
