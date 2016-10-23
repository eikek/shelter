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

(deftest verify-and-update-test
  (account/account-register conn "eike" "test")
  (account/app-set conn {:appid "wiki" :appname "a wiki"})
  (testing "verify account"
    (is (= false (verify-and-update "eike" "other")))
    (is (= false (verify-and-update "eike" "test" "mail")))
    (is (= false (verify-and-update "eike" "test" "wiki"))))
  (testing "update details"
    (let [details (shelter.store/with-conn conn
                    (account/account-details-get conn "eike"))]
      (is (= 3 (:failedlogins details)))
      (is (= 0 (:logincount details)))
      (is (= false (:locked details)))
      (is (= nil (:lastlogin details)))
      (is (= nil (:name details)))
      (is (= nil (:email details)))))
  (testing "verify account"
    (is (verify-and-update "eike" "test")))
  (testing "update details"
    (let [details (shelter.store/with-conn conn
                    (account/account-details-get conn "eike"))]
      (is (= 1 (:logincount details)))
      (is (= 3 (:failedlogins details)))
      (is (> (:lastlogin details) 0))
      (is (= nil (:name details)))
      (is (= nil (:email details))))
    (verify-and-update "eike" "test")
    (let [details (shelter.store/with-conn conn
                    (account/account-details-get conn "eike"))]
      ;; should not update after quick subsequent calls
      (is (= 1 (:logincount details)))
      (is (= 3 (:failedlogins details))))))

(deftest account-enabled-test
  (account/account-register conn "eike" "test")
  (account/account-register conn "linda" "test")
  (account/account-register conn "bob")
  (account/app-set conn {:appid "wiki" :appname "a wiki"})
  (account/app-enable conn "eike" "wiki")
  (testing "enabled or not enabled"
    (is (= true (account-enabled? "eike")))
    (is (= true (account-enabled? "linda")))
    (is (= true (account-enabled? "eike" "wiki")))
    (is (= false (account-enabled? "linda" "wiki")))
    (is (= false (account-enabled? "bob")))
    (is (= false (account-enabled? "bob" "wiki")))
    (is (= false (account-enabled? "james")))))
