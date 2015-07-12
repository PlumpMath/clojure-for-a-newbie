(defproject clojure-newbie "0.1.0-SNAPSHOT"
  :license {:name "GPL v.3"
            :url "http://www.gnu.org/licenses/gpl-3.0.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot clojure-newbie.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
