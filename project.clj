(defproject forum "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main forum.core
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [com.datomic/datomic-free "0.8.4111"]
                 [compojure "1.1.5"]
                 [crate "0.2.4"]
                 [incanter/incanter-core "1.3.0"]
                 [hiccup "1.0.3"]
                 [ring/ring-jetty-adapter "1.1.8"]
                 [enlive "1.1.1"]])
