(defproject fourtytoo/demyjtify "0.1.0-SNAPSHOT"
  :description "Milter library"
  :url "https://github.com/fourtytoo/demyjtify"
  :license {:name "LGPL"
            :url "http://www.gnu.org/copyleft/lesser.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [onelog "0.5.0"]
                 [fourtytoo/bnb4clj "0.1.0-SNAPSHOT"]]
  :profiles {:uberjar {:aot :all}})
