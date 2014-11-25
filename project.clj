(defproject shelter "0.1.0-SNAPSHOT"
  :description "Manage user account data"
  :url "https://gitub.com/eikek/shelter"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.6.0"]
                 [org.mindrot/jbcrypt "0.3m"]
                 [com.lambdaworks/scrypt "1.4.0"]
                 [org.clojure/tools.nrepl "0.2.6"]
                 [ring/ring-core "1.3.1"]
                 [ring/ring-devel "1.3.1"]
                 [ring/ring-jetty-adapter "1.3.1"]
                 [ring/ring-json "0.3.1"]
                 [compojure "1.2.1"]
                 [org.clojure/java.jdbc "0.3.6"]
                 [org.xerial/sqlite-jdbc "3.8.7"]
                 ]
  :main ^:skip-aot shelter.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
