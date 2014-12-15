(ns shelter.core-test
  (:require [clojure.test :refer :all]
            [shelter.config :as config]
            [shelter.account :as account]
            [shelter.core :refer :all]))

(def conn nil)

(defn with-new-database [f]
  (let [file "core-tests.db"]
    (java.nio.file.Files/deleteIfExists
     (.toPath (clojure.java.io/as-file file)))
    (shelter.migration/migrate-db file)
    (config/set {:database file})
    (shelter.store/with-conn c
      (def conn c)
      (f))))

(use-fixtures :each with-new-database)

(deftest verify-test
  (account/account-register conn "eike" "test")
  (account/app-set conn {:appid "wiki" :appname "a wiki"})
  (testing "verify account"
    (is (verify "eike" "test"))
    (is (= false (verify "eike" "other")))
    (is (= false (verify "eike" "test" "mail")))
    (is (= false (verify "eike" "test" "wiki"))))
  (testing "verify against apps"
    (account/app-enable conn "eike" "wiki")
    (is (= true (verify "eike" "test" "wiki"))))
  (testing "verify against app specific"
    (account/secret-set-password conn "eike" "super" "wiki")
    (is (verify "eike" "super" "wiki"))
    (is (= false (verify "eike" "test" "wiki")))))
