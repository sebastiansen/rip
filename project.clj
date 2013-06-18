(defproject rip "0.0.10"
  :description "REST in Peace, a framework for RESTful API development."
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
                   [[korma "0.3.0-RC5"]
                    [http-kit "2.0.1"  :exclusions [org.clojure/clojure]]
                    [ring-mock "0.1.3" :exclusions [org.clojure/clojure]]]}}
  :aot [rip.RipException])
