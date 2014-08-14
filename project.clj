(defproject forum "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
;;  :main f.core
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [com.datomic/datomic-pro "0.9.4815.12"]
                 [compojure "1.2.0-SNAPSHOT"]
                 [org.clojure/tools.nrepl "0.2.3"]
                 ;; [crate "0.2.4"]
                 [hiccup "1.0.3"]
                 [incanter/incanter-core "1.3.0"]
                 [enlive "1.1.4"]
                 [ring/ring-jetty-adapter "1.1.7"]]
  :ring {:handler f.handler/app}
  :repl-options {:port 9995}
  :repositories {"my.datomic.com" {:url "https://my.datomic.com/repo"}}
  :uberjar-exclusions [#"(?i)^META-INF/[^/]*\.SF$"])
