(defproject demijtify "0.1.0-SNAPSHOT"
  :description "Milter library"
  :url "https://github.com/fourtytoo/demyjtify"
  :license {:name "LGPL"
            :url "http://www.gnu.org/copyleft/lesser.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.logging "0.3.1"]
                 [log4j/log4j "1.2.17"]
                 [clj-logging-config "1.9.12"]]
  :profiles {:uberjar {:aot :all}
             :dev {:dependencies [[nrepl-inspect "0.4.1"]]
                   :plugins [[cider/cider-nrepl "0.9.0-SNAPSHOT"]]}})
