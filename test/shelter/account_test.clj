(ns shelter.account-test
  (:require [clojure.test :refer :all]
            [clojure.java.jdbc :as sql]
            [shelter.config :as config]
            [shelter.secret :as secret]
            [shelter.account :refer :all]))

(def conn nil)

(defn with-new-database [f]
  (let [file "account-tests.db"]
    (java.nio.file.Files/deleteIfExists
     (.toPath (clojure.java.io/as-file file)))
    (shelter.migration/migrate-db file)
    (config/set {:database file})
    (shelter.store/with-conn c
      (def conn c)
      (f))))

(use-fixtures :each with-new-database)

(defn insert-account [name]
  (sql/insert! conn :shelter_account {:login name }))

(defn string< [s1 s2]
  (> 0 (.compareTo s1 s2)))

(deftest login-tests
  (testing "non existing account"
    (is (= false (login-exists? conn "eike")))
    (is (= false (alias-exists? conn "eikek"))))
  (testing "alias crud"
    (insert-account "eike")
    (is (= {:loginalias "eikek" :login "eike"}
           (alias-add conn "eike" "eikek")))
    (is (= true (alias-exists? conn "eikek")))
    (is (= ["eikek"] (alias-list conn "eike")))
    (is (= nil (alias-add conn "noname" "noname2")))
    (is (= true (alias-remove conn "eikek")))
    (is (= false (alias-exists? conn "eikek"))))
  (testing "alias add and account exists"
    (insert-account "mary")
    (is (= nil (alias-add conn "mary" "eike")))))

(deftest app-tests
  (testing "non existing apps"
    (is (= false (app-exists? conn "mail")))
    (is (= [] (app-list conn))))
  (testing "app crud"
    (is (= true (app-set conn {:appid "mail" :appname "a mail app"})))
    (is (= [{:appid "mail", :appname "a mail app", :url nil, :description nil}]
           (app-list conn)))
    (is (= true (app-set conn {:appid "wiki" :appname "Wiki" :url "http://wiki.com"})))
    (is (= {:appid "wiki" :appname "Wiki" :url "http://wiki.com" :description nil}
           (app-get conn "wiki")))
    (is (= true (app-set conn {:appid "wiki" :url "http://wiki.org"})))
    (is (= {:appid "wiki" :appname "Wiki" :url "http://wiki.org" :description nil}
           (app-get conn "wiki")))
    (is (= true (app-exists? conn "mail")))
    (is (= true (app-remove conn "mail")))
    (is (= false (app-exists? conn "mail"))))
  (testing "app enable/disable"
    (insert-account "eike")
    (app-set conn {:appid "mail" :appname "a mail app"})
    (is (= [{:appid "mail", :appname "a mail app", :url nil, :description nil}
            {:appid "wiki", :appname "Wiki", :url "http://wiki.org", :description nil}]
           (sort #(string< (:appid %1) (:appid %2)) (app-list conn))))
    (is (= [] (app-list-enabled conn "eike")))
    (is (= true (app-enable conn "eike" "mail")))
    (is (= true (app-disable conn "eike" "mail"))))
  (testing "app-enabled? for non-existing app"
    (is (= false (app-enabled? conn "eike" "wiki"))))
  (testing "app-enabled? for alias"
    (alias-add conn "eike" "human")
    (app-enable conn "eike" "mail")
    (is (= true (app-enabled? conn "eike" "mail")))
    (is (= true (app-enabled? conn "human" "mail")))))

(deftest app-cascade-remove
  (testing "remove app"
    (insert-account "eike")
    (app-set conn {:appid "mail" :appname "a mail app"})
    (app-enable conn "eike" "mail")
    (app-remove conn "mail")
    (is (= false (app-exists? conn "mail")))
    (is (= false (app-enabled? conn "eike" "mail")))))

(deftest secret-set-test
  (let [secret (secret/make-password (secret/random-string 10)
                                     :salt_rounds 8)]
    (insert-account "eike")
    (testing "wrong login"
      (is (= nil (secret-set conn "bla" secret))))
    (testing "wrong app"
      (is (= nil (secret-set conn "eike" secret "noapp"))))
    (testing "update secret"
      (is (= false (secret-exists? conn "eike")))
      (is (= true (secret-set conn "eike" secret)))
      (is (= true (secret-exists? conn "eike"))))
    (testing "update secret for app"
      (is (= false (secret-exists? conn "eike" "mail"))))
    (testing "reset password"
      (is (string? (secret-reset-password conn "eike"))))))

(deftest secret-get-test
  (insert-account "eike")
  (insert-account "eike2")
  (insert-account "eike3")
  (app-set conn {:appid "mail" :appname "a mail app"})
  (secret-set conn "eike2" (secret/make-password "test"))
  (secret-set conn "eike3" (secret/make-password "test"))
  (secret-set conn "eike2" (secret/make-password "test") "mail")
  (testing "get some secrets"
    (let [secrets (secret-get conn "eike3" "mail")]
      (is (= 1 (count secrets)))
      (doseq [secret secrets]
        (is (string? (:data secret)))
        (is (= :password (:type  secret)))
        (is (= :bcrypt (:hash secret)))))
    (let [secrets (secret-get conn "eike3")]
      (is (= 1 (count secrets)))
      (doseq [secret secrets]
        (is (string? (:data secret)))
        (is (= :password (:type  secret)))
        (is (= :bcrypt (:hash secret))))))
  (testing "get some secrets"
    (let [secrets (secret-get conn "eike2")]
      (is (= 1 (count secrets)))
      (doseq [secret secrets]
        (is (string? (:data secret)))
        (is (= :password (:type  secret)))
        (is (= :bcrypt (:hash secret))))))
  (testing "get empty secrets"
    (is (= [] (secret-get conn "eike")))))

(deftest account-list-test
  (insert-account "eike")
  (insert-account "john")
  (alias-add conn "eike" "eikek")
  (app-set conn {:appid "mail" :appname "a mail app"})
  (app-enable conn "eike" "mail")
  (testing "list-account"
    (let [list (account-list conn)]
      (is (= [{:login "eike", :apps ["mail"], :aliases ["eikek"]}
              {:login "john", :apps [], :aliases []}]
             list)))))

(deftest account-properties
  (insert-account "eike")
  (testing "set properties"
    (is (= true (account-properties-set conn "eike" nil {:name "Eike" :homepage "http://test.com"})))
    (is (= true (account-properties-set conn "eike" nil {:name "Eike" :homepage "http://other.com"})))
    (is (= 1 (-> (sql/query conn ["select count(*) as cnt from shelter_account_property where login=? and appid is null"
                                   "eike"])
               (first) (:cnt)))))
  (testing "get properties"

    (is (= {:name "Eike" :homepage "http://other.com" :email nil :appid nil :login "eike"}
           (account-properties-get conn "eike")))))

(deftest account-details-get-test
  (insert-account "eike")
  (testing "get empty details"
    (is (= {:failedlogins 0,
            :logincount 0,
            :lastlogin nil,
            :email nil,
            :name nil,
            :locked true}
           (account-details-get conn "eike"))))
  (testing "set details by value"
    (is (= true (account-details-set conn "eike" {:logincount 10})))
    (is (= {:failedlogins 0,
            :logincount 10,
            :lastlogin nil,
            :email nil,
            :name nil,
            :locked true}
           (account-details-get conn "eike"))))
  (testing "set locked bool"
    (is (= true (account-details-set conn "eike" {:locked false})))
    (is (= {:failedlogins 0,
            :logincount 10,
            :lastlogin nil,
            :email nil,
            :name nil,
            :locked false}
           (account-details-get conn "eike"))))
  (testing "set details via fn"
    (is (= true (account-details-set conn "eike" #(update-in % [:logincount] inc))))
    (is (= {:failedlogins 0,
            :logincount 11,
            :lastlogin nil,
            :email nil,
            :name nil,
            :locked false}
           (account-details-get conn "eike")))))

(deftest account-get-test
  (insert-account "eike")
  (testing "get empty account"
    (is (= {:name "eike",
            :details {:failedlogins 0,
                      :logincount 0,
                      :lastlogin nil,
                      :email nil,
                      :name nil,
                      :locked true},
            :aliases [],
            :apps (),
            :profiles {:default {:secrets [], :properties nil}}}
           (account-get conn "eike")))
    (is (account-locked? conn "eike"))))

(deftest account-register-test
  (testing "register locked account"
    (is (account-register conn "mary"))
    (is (= "mary" (:name (account-get conn "mary"))))
    (is (:locked (account-details-get conn "mary"))))
  (testing "register active account"
    (is (account-register conn "john" "testpw"))
    (is (= "john" (:name (account-get conn "john"))))
    (is (not (:locked (account-details-get conn "john")))))
  (testing "try existing account"
    (is (= nil (account-register conn "john")))))
