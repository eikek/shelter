(defproject shelter "0.3.0-SNAPSHOT"
  :description "Manage user account data"
  :url "https://gitub.com/eikek/shelter"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.nrepl "0.2.12"]
                 [org.mindrot/jbcrypt "0.3m"]
                 [com.lambdaworks/scrypt "1.4.0"]
                 [ring/ring-core "1.5.0"]
                 [ring/ring-devel "1.5.0"]
                 [ring/ring-jetty-adapter "1.5.0"]
                 [ring/ring-json "0.4.0"]
                 [ring/ring-mock "0.3.0"]
                 [compojure "1.5.1"]
                 [org.clojure/java.jdbc "0.6.1"]
                 [org.xerial/sqlite-jdbc "3.14.2.1"]
                 ]
  :main ^:skip-aot shelter.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
