(defproject euler "1.0.0-SNAPSHOT"
  :description "Project Euler solutions"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [com.github.kyleburton/clj-bloom "1.0.1"]]
;  :warn-on-reflection true
  :dev-dependencies [[swank-clojure "1.2.1"]]
  :jvm-opts ["-server"])
;, "-agentpath:/home/eduard/Downloads/YourKit.Java.Profiler.v9.0/YourKit.Java.Profiler.v9.0.Linux/yjp-9.0.0/bin/linux-x86-64/libyjpagent.so"])
