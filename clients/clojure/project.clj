(defproject extremecarpaccio "0.1.0-SNAPSHOT"
  :description "Minimal Clojure client for the Extreme Carpaccio kata"
  :url "https://github.com/dlresende/extreme-carpaccio"
  :license {:name "MIT"}
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.12.6"]
                 [compojure "1.7.2"]
                 [ring/ring-core "1.15.5"]
                 [ring/ring-jetty-adapter "1.15.5"]]
  :main extremecarpaccio.server
  :profiles {:uberjar {:aot :all}})
