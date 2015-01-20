(defproject forum "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main f.core
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [com.datomic/datomic-pro "0.9.5078"]
                 [compojure "1.2.0-SNAPSHOT"]
                 [org.clojure/tools.nrepl "0.2.3"]
                 ;; [crate "0.2.4"]
                 [org.postgresql/postgresql "9.3-1102-jdbc41"]
                 [hiccup "1.0.3"]
                 [incanter/incanter-core "1.3.0"]
                 [enlive "1.1.4"]
                 [ring/ring-jetty-adapter "1.1.7"]]
  :ring {:handler f.handler/app}
  :repl-options {:port 9995}
  :aot [f.core]
  ;;:repositories {"my.datomic.com" {:url "https://my.datomic.com/repo"}}
  :uberjar-exclusions [#"(?i)^META-INF/[^/]*\.SF$"])
