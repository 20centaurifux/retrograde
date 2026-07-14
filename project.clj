(defproject de.dixieflatline/retrograde "0.1.0-SNAPSHOT"
  :description "A key-value database with stateful versioning and time-dependent degeneration"
  :url "https://github.com/20centaurifux/retrograde"
  :license {:name "AGPLv3"
            :url "https://www.gnu.org/licenses/agpl-3.0"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.cache "1.2.263"]
                 [org.clojure/test.check "1.1.3"]
                 [com.github.seancorfield/next.jdbc "1.3.1118"]
                 [com.github.seancorfield/honeysql "2.7.1399"]]
  :target-path "target/%s"
  :aot nil
  :profiles {:test {:dependencies [[org.xerial/sqlite-jdbc "3.53.2.0"]
                                   [tortue/spy "2.15.0"]]}})