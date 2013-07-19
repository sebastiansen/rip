(defproject sebastiansen/rip "0.1.0"
  :description "REST in Peace, a framework for RESTful applications."
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [compojure "1.1.5"]
                 [cheshire "5.0.2"]
                 [org.clojure/data.xml "0.0.7"]
                 [hiccup "1.0.3"]
                 [clout "1.1.0"]
                 [ring/ring-core "1.1.8"]
                 [com.twinql.clojure/clj-conneg "1.1.0"]
                 [com.taoensso/tower "1.7.1"]]
  :profiles {:dev {:dependencies
                   [[ring-mock "0.1.3" :exclusions [org.clojure/clojure]]]}}
  :plugins [[codox "0.6.4"]])
