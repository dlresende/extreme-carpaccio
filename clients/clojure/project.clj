(defproject extremecarpaccio "0.1.0-SNAPSHOT"
  :description "Dumbed down Clojure example code for Extreme Carpaccio kata"
  :url "https://github.com/dlresende/extreme-carpaccio"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [compojure "1.6.1"]
                 [ring "1.8.0"]
                 [ring/ring-mock "0.4.0"]
                 [ring-middleware-format "0.7.4"]]
  :plugins [[lein-ring "0.12.5"]
            [lein-midje "3.2.2"]]
  :ring {:handler extremecarpaccio.core/my-app}
  :profiles {:dev {:dependencies [[midje "1.9.9"]]}})




