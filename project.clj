(defproject euler "1.0.0-SNAPSHOT"
  :description "Project Euler solutions"
  :dependencies [[org.clojure/clojure "1.2.0-beta1"]
                 [org.clojure/clojure-contrib "1.2.0-beta1"]]
;  :warn-on-reflection true
  :dev-dependencies [[swank-clojure "1.2.1"]]
  :jvm-opts ["-Xmx1g", "-server", "-XX:+UseParallelGC"])

;, "-agentpath:/home/eduard/Downloads/YourKit.Java.Profiler.v9.0/YourKit.Java.Profiler.v9.0.Linux/yjp-9.0.0/bin/linux-x86-64/libyjpagent.so"])
