(ns shelter.rest-test
  (:require
   [clojure.test :refer :all]
   [shelter.config :as config]
   [shelter.account :as account]
   [shelter.secret :as secret]
   [shelter.rest :refer :all]
   [compojure.core :as web]
   [compojure.route :as route]
   [compojure.core :refer :all]
   [ring.middleware.cookies :as cookies]
   [ring.middleware.keyword-params :as kwp]
   [ring.middleware.json :as json]
   [ring.mock.request :refer :all]))


(def conn nil)

(defn with-new-database [f]
  (let [file "rest-tests.db"]
    (java.nio.file.Files/deleteIfExists
     (.toPath (clojure.java.io/as-file file)))
    (shelter.migration/migrate-db file)
    (config/set {:database file})
    (shelter.store/with-conn c
      (def conn c)
      (f)
      (java.nio.file.Files/deleteIfExists
       (.toPath (clojure.java.io/as-file file))))))

(use-fixtures :each with-new-database)

(deftest token-tests
  (account/account-register conn "jonas")
  (testing "no token without password"
    (is (= nil (make-authtoken "jonas"))))
  (testing "token with password"
    (account/account-register conn "juli" "test")
    (is (string? (make-authtoken "juli"))))
  (testing "no token with locked account"
    (account/account-set-locked conn "juli" true)
    (is (= nil (make-authtoken "juli"))))
  (testing "verify success"
    (account/account-set-locked conn "juli" false)
    (account/account-set-locked conn "jonas" false)
    (account/secret-update-password conn "jonas" "test")
    (let [token1 (make-authtoken "juli")
          token2 (make-authtoken "jonas")]
      (is (= true (verify-authtoken token1)))
      (is (= true (verify-authtoken token2)))))
  (testing "verify fail"
    (let [token1 (.replace (make-authtoken "juli") "juli" "john")
          token2 (.replaceAll (make-authtoken "jonas") "[0-9]+" "123123")]
      (is (= false (verify-authtoken token1)))
      (is (= false (verify-authtoken token2))))))

(deftest setpassword-form-test
  (account/account-register conn "jonas" "test")
  (let [token (make-authtoken "jonas")
        handler (web/routes (make-setpassword-form-handler (fn [a b c [d]] true)))]
    (is (= {:status 400,
            :headers {"Content-Type" "application/json; charset=utf-8"},
            :body "{\"success\":false,\"message\":\"Invalid request.\"}"}
           (handler (-> (request :post "/setpass/form")
                      (assoc :cookies {(config/get :cookie-name) {:value token}})))))
    (is (= {:status 200,
            :headers {"Content-Type" "application/json; charset=utf-8"},
            :body "{\"success\":true}"}
           (handler (-> (request :post "/setpass/form" {:login "eike" :password "test" :newpassword "test2"})
                      (assoc :cookies {(config/get :cookie-name) {:value token}})))))
    (is (= {:status 403,
            :headers {"Set-Cookie" '("shelterauth=delete;Max-Age=1")},
            :body "Not authenticated."}
           (handler (-> (request :post "/setpass/form" {:login "eike" :password "test" :newpassword "test2"})
                      (assoc :cookies {(config/get :cookie-name) {:value "sdasd"}})))))))


(deftest verify-form-test
  (let [handler (web/routes (make-verify-form-handler (fn [a b & [c]] true)  ;; function that verifies user
                                                    (fn [a b] nil)))]      ;; function that creates cookie value
    (is (= {:status 400,
            :headers {"Content-Type" "application/json; charset=utf-8"},
            :body "{\"success\":false,\"message\":\"Invalid request.\"}"}
           (handler (request :post "/verify/form"))))
    (is (= {:status 200,
            :headers {"Content-Type" "application/json; charset=utf-8"},
            :body "{\"success\":true}"}
           (handler (request :post "/verify/form" {:login "eike" :password "test"})))))
  (let [handler (web/routes (make-verify-form-handler (fn [a b & [c]] false)  ;; function that verifies user
                                                    (fn [a b] nil)))]       ;; function that creates cookie value
    (is (= {:status 400,
            :headers {"Content-Type" "application/json; charset=utf-8"},
            :body "{\"success\":false,\"message\":\"Invalid request.\"}"}
           (handler (request :post "/verify/form"))))
    (is (= {:status 401,
            :headers {"Content-Type" "application/json; charset=utf-8"},
            :body "{\"success\":false,\"message\":\"Request failed.\"}"}
           (handler (request :post "/verify/form" {:login "eike" :password "test"}))))))

(deftest verify-json-test
  (let [handler (web/routes (make-verify-json-handler (fn [a b [c]] true) ;; function that verifies user
                                                    (fn [a b] nil)))]   ;; function that creates cookie value
    (is (= {:status 400,
            :headers {"Content-Type" "application/json; charset=utf-8"},
            :body "{\"success\":false,\"message\":\"Invalid request.\"}"}
           (handler (-> (request :post "/verify/json")
                      (content-type "application/json; charset=utf-8")))))
    (is (= {:status 200,
            :headers {"Content-Type" "application/json; charset=utf-8"},
            :body "{\"success\":true}"}
           (handler (-> (request :post "/verify/json")
                      (content-type "application/json; charset=utf-8")
                      (body "{\"login\":\"eike\",\"password\":\"test\"}"))))))
  (let [handler (web/routes (make-verify-json-handler (fn [a b [c]] false) ;; function that verifies user
                                                    (fn [a b] nil)))]    ;; function that creates cookie value
    (is (= {:status 400,
            :headers {"Content-Type" "application/json; charset=utf-8"},
            :body "{\"success\":false,\"message\":\"Invalid request.\"}"}
           (handler (-> (request :post "/verify/json")
                      (content-type "application/json; charset=utf-8")))))
    (is (= {:status 401,
            :headers {"Content-Type" "application/json; charset=utf-8"},
            :body "{\"success\":false,\"message\":\"Request failed.\"}"}
           (handler (-> (request :post "/verify/json")
                      (content-type "application/json; charset=utf-8")
                      (body "{\"login\":\"eike\",\"password\":\"test\"}")))))))
