(defproject pushkin "0.1.0-SNAPSHOT"
  :description "plays a bit of Go"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [potemkin "0.1.6"]
                 [criterium "0.3.0"]
                 [useful "0.8.5"]]
  :warn-on-reflections true
  :source-paths ["src" "../duel/src"]
  :jvm-opts ["-server"])
