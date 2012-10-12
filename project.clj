(defproject pushkin "0.1.0-SNAPSHOT"
  :description "plays a bit of Go"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [potemkin "0.1.6"]
                 [criterium "0.3.0"]
                 [useful "0.8.5"]
                 [org.uncommons.maths/uncommons-maths "1.2.2" :exclusions [jfree/jfreechart]]]
  :warn-on-reflection true
  :source-paths ["src" "../duel/src"]
  :test-selectors {:default #(not (some #{:stress :benchmark} %))
                   :stress :stress
                   :benchmark :benchmark}
  :jvm-opts ["-server" "-Xmx4g"])
