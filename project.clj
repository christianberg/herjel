(defproject herjel "0.1.0-SNAPSHOT"
  :description "A ray tracer in Clojure"
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :profiles {:dev {:dependencies [[midje "1.5.0"]]}}
  :plugins [[lein-midje "3.0.0"]])