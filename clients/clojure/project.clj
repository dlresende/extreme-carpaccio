(defproject extremecarpaccio "0.1.0-SNAPSHOT"
            :description "Dumbed down Clojure example code for Extreme Carpaccio kata"
            :url "https://github.com/dlresende/extreme-carpaccio"
            :min-lein-version "2.0.0"
            :dependencies [[org.clojure/clojure "1.6.0"]
                           [compojure "1.3.1"]
                           [ring "1.3.2"]
                           [ring/ring-mock "0.2.0"]]
            :plugins [[lein-ring "0.8.13"]]
            :ring {:handler extremecarpaccio.core/my-app})
