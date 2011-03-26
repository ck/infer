(defproject infer "1.0.1-SNAPSHOT"
  :description "inference and machine learning for clojure"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
		 [clojure-csv/clojure-csv "1.1.0"]
		 [org.apache.commons/commons-math "2.0"]
                 [ujmp-complete "0.2.4"]
		 [clj-sys/plumbing "0.1.3-SNAPSHOT"]
		 [colt/colt "1.2.0"]
		 [incanter/parallelcolt "0.9.4"]]
  :dev-dependencies [[swank-clojure "1.2.0"]]
  :java-source-path "src/jvm"
  :repositories {"snapshots" "http://mvn.getwoven.com/repos/woven-public-snapshots"
                 "releases" "http://mvn.getwoven.com/repos/woven-public-releases"
                 "oracle" "http://download.oracle.com/maven/"})
