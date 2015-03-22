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
    (config/set {:token-validity (* 20 60 1000)})
    (shelter.store/with-conn c
      (def conn c)
      (f)
      (java.nio.file.Files/deleteIfExists
       (.toPath (clojure.java.io/as-file file))))))

(use-fixtures :each with-new-database)

(deftest token-tests
  (account/account-register conn "jonas")
  (account/account-register conn "juli" "test")
  (account/app-set conn {:appid "mail" :appname "email"})
  (account/app-set conn {:appid "wiki" :appname "Wiki"})
  (account/app-enable conn "jonas" "mail")
  (testing "verifying weird values"
    (is (= false (verify-authtoken "asdasd"))))
  (testing "treat empty app like nil"
    (with-redefs [secret/random-string (fn [n] "abc")
                  shelter.rest/current-millis (fn [] 123123123)]
      (is (= (make-authtoken "juli")
             (make-authtoken "juli" "")))))
  (testing "no token without password"
    (is (= nil (make-authtoken "jonas"))))
  (testing "token with password"
    (is (string? (make-authtoken "juli"))))
  (testing "verify success"
    (account/account-set-locked conn "juli" false)
    (account/account-set-locked conn "jonas" false)
    (account/secret-set-password conn "jonas" "test")
    (let [token1 (make-authtoken "juli")
          token2 (make-authtoken "jonas")]
      (is (= true (verify-authtoken token1)))
      (is (= true (verify-authtoken token2)))))
  (testing "verify for app"
    (account/secret-set-password conn "jonas" "testmail" "mail")
    (let [token (make-authtoken "jonas" "mail")
          tokend (make-authtoken "jonas")]
      (is (= true (verify-authtoken token "mail")))
      (is (= false (verify-authtoken token :default)))
      (is (= true (verify-authtoken token "mail")))
      (is (= true (verify-authtoken tokend)))
      (is (= true (verify-authtoken tokend :default)))
      (is (= false (verify-authtoken token :default)))
      (is (= false (verify-authtoken token "wiki")))
      (is (= false (verify-authtoken tokend "wiki")))
      (is (= false (verify-authtoken tokend "mail"))))) ;;has separate password for mail
  (testing "verify fail"
    (let [token1 (.replace (make-authtoken "juli") "juli" "john")
          token2 (.replaceAll (make-authtoken "jonas") "[0-9]+" "123123")]
      (is (= false (verify-authtoken token1)))
      (is (= false (verify-authtoken token2))))))

(deftest setpassword-form-test
  (account/account-register conn "jonas" "test")
  (account/secret-set-password conn "jonas" "testmail" "mail")
  (let [token (make-authtoken "jonas")
        handler (web/routes (make-setpassword-form-handler (fn [a b c [d]] true)
                                                           (fn [a b] nil)))]      ;; function that creates cookie value
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
    (is (= {:status 401,
            :headers {"Set-Cookie" '("shelterauth=;HttpOnly;Path=/;Max-Age=1")},
            :body "Unauthorized."}
           (handler (-> (request :post "/setpass/form" {:login "eike" :password "test" :newpassword "test2"})
                        (assoc :cookies {(config/get :cookie-name) {:value "sdasd"}})))))
    (is (= {:status 401,
            :headers {},
            :body "Unauthorized."}
           (handler (request :post "/setpass/form" {:login "eike" :password "test" :newpassword "test2"}))))))

(defn- is-success-with-cookie [r]
  (let [nextcookie (get-in r [:headers "Set-Cookie"])]
    (is (= 200 (:status r)))
    (is (not-empty nextcookie))
    (is (= "{\"success\":true}" (:body r)))))

(deftest verify-cookie-test
  (account/app-set conn {:appid "mail" :appname "email"})
  (account/account-register conn "jonas" "test")
  (account/app-enable conn "jonas" "mail")
  (account/secret-set-password conn "jonas" "testmail" "mail")
  (let [handler (web/routes (make-verify-cookie-handler))
        token   (make-authtoken "jonas")
        token-mail  (make-authtoken "jonas" "mail")
        token-wiki  (make-authtoken "jonas" "wiki")]
    (is-success-with-cookie
     (handler (-> (request :get "/verify/cookie" {:app "mail"})
                  (assoc :cookies {(config/get :cookie-name) {:value token-mail}}))))
    (is-success-with-cookie
     (handler (-> (request :get "/verify/cookie")
                  (assoc :cookies {(config/get :cookie-name) {:value token-mail}})
                  (assoc :headers {"x-shelter-app" "mail"}))))
    (is (= {:status 401,
            :headers {"Set-Cookie" '("shelterauth=;HttpOnly;Path=/;Max-Age=1")},
            :body "Unauthorized."}
           (handler (-> (request :get "/verify/cookie" {:app "mail"})
                        (assoc :cookies {(config/get :cookie-name) {:value token}})))))
    (is (= {:status 401,
            :headers {},
            :body "Unauthorized."}
           (handler (-> (request :get "/verify/cookie" {:app "mail"})
                        (assoc :cookies {(config/get :cookie-name) {:value token-wiki}})))))
    (is-success-with-cookie
     (handler (-> (request :get "/verify/cookie")
                  (assoc :cookies {(config/get :cookie-name) {:value token}}))))
    (is (= {:status 401, :headers {}, :body "Unauthorized."}
           (handler (request :get "/verify/cookie"))))
    (is (= {:status 401,
            :headers {"Set-Cookie" '("shelterauth=;HttpOnly;Path=/;Max-Age=1")},
            :body "Unauthorized."}
           (handler (-> (request :get "/verify/cookie")
                        (assoc :cookies {(config/get :cookie-name) {:value "bla"}})))))
    (is-success-with-cookie
     (handler (-> (request :get "/verify/cookie")
                  (assoc :cookies {(config/get :cookie-name) {:value token}}))))))

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

(deftest verify-logout-test
  (let [handler (web/routes (make-logout-handler))]
    (is (= {:status 200
            :headers {"Set-Cookie" '("shelterauth=;HttpOnly;Path=/;Max-Age=1")
                      "Content-Type" "application/json; charset=utf-8"}
            :body "{\"success\":true}"}
           (handler (request :get "/logout"))))))
