(defproject stats-generator "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "MIT License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/data.csv "0.1.4"]]
  :main ^:skip-aot stats-generator.main
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}

  :aliases {"queens" ["run" "-m" "stats-generator.queens/init"]})
