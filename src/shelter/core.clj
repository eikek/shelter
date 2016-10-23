;;; core.clj -- shelter main entry point

;; This module contains functions intended for user interaction from
;; within a repl.

(ns shelter.core
  (:require
   [shelter.config :as config]
   [shelter.store :as store]
   [shelter.account :as account]
   [shelter.secret :as secret]
   [shelter.rest :as rest]
   [compojure.core :refer [ANY GET POST]]
   [clojure.java.io :as f]
   [clojure.tools.nrepl.server :as nrepl])
  (:gen-class :main true))

;; default values
(config/set {:nrepl-port 7989})

(defonce ^:private nrepl-server (atom nil))

(defn verify
  "Return true if name and password can be verified against the user
  database.

  If APP is given and the account has a separate password for APP it
  is checked against this password. Otherwise the default password of
  the account is used if the account is enabled for APP."
  [name password & [app]]
  (store/with-conn conn
    (cond (account/account-locked? conn name) false
          (and (not-empty app) (not (account/app-enabled? conn name app))) false
          :else (let [appsecrets (account/secret-get conn name app)
                      secrets (if (empty? appsecrets)
                                (account/secret-get conn name)
                                appsecrets)
                      result (secret/verify secrets password)]
                  (or (some second result) false)))))


(defn verify-and-update
  "Run `verify' and update account details according to the result."
  [name password & [app]]
  (let [result (verify name password app)]
    (store/with-conn conn
      (account/account-details-set
       conn name
       (fn [data]
         (conj data
               (if result {:lastlogin (System/currentTimeMillis)})
               (if result
                 {:logincount (inc (:logincount data))}
                 {:failedlogins (inc (:failedlogins data))})))))
    result))

(defn set-application
  "Adds or updates an application given as map."
  [app]
  (store/with-conn conn
    (account/app-set conn app)))

(defn list-applications
  "List all applications"
  []
  (store/with-conn conn
    (account/app-list conn)))

(defn register-account
  "Registers a new account. If PASS is given, the account is active,
  otherwise locked."
  [login & [pass details]]
  (store/with-conn conn
    (account/account-register conn login pass details)))

(defn account-enabled? [name & [app]]
  "Check whether an account LOGIN exists, is not locked and optionally
  if it is enabled for APP."
  (store/with-conn conn
    (cond
      (not (account/account-exists? conn name)) false
      (account/account-locked? conn name) false
      (and (not-empty app) (not (account/app-enabled? conn name app))) false
      :else true)))

(defn list-accounts
  "List all accounts."
  []
  (store/with-conn conn
    (account/account-list conn)))

(defn show-account
  "Shows all details about an account."
  [login]
  (store/with-conn conn
    (account/account-get conn login)))

(defn set-password
  "Set a new password for an account and app."
  [login password & [appid]]
  (store/with-conn conn
    (account/secret-set-password conn login password appid)))

(defn reset-password
  "Resets the password for an account and app to a random one."
  [login & [appid]]
  (store/with-conn conn
    (account/secret-reset-password conn login appid)))

(defn set-account-locked
  "Set the account locked/not locked."
  [login locked]
  (store/with-conn conn
    (account/account-set-locked conn login locked)))

(defn enable-app
  "Enables an app for an account."
  [login appid]
  (store/with-conn conn
    (account/app-enable conn login appid)))

(defn disable-app
  "Disables an app for an account."
  [login appid]
  (store/with-conn conn
    (account/app-disable conn login appid)))

(defn add-alias
  "Adds a new alias for an account."
  [login alias]
  (store/with-conn conn
    (account/alias-add conn login alias)))

(defn remove-alias
  "Removes an alias mapping for an account."
  [alias]
  (store/with-conn conn
    (account/alias-remove conn alias)))

(defn add-verify-routes
  "Add POST routes to the rest handler that verifies account
  credentials given in the url form or json body. Success or failure
  is encoded in the http status code and additionally in the body
  using a map to be encoded as json."
  []
  (rest/add-routes
   :verifycookie (ANY "/api/verify/cookie" request
                   ((rest/make-verify-cookie-handler) request))
   :verifyform (POST "/api/verify/form" request
                 ((rest/make-verify-form-handler verify-and-update) request))
   :verifyjson (POST "/api/verify/json" request
                 ((rest/make-verify-json-handler verify-and-update) request))))

(defn- user-set-password
  "Sets a NEWPW for LOGIN if verification is successful using OLDPW."
  [login oldpw newpw & [appid]]
  (if (verify login oldpw appid)
    (store/with-conn conn
      (account/secret-set-password conn login newpw appid))))

(defn- user-force-set-password
  "Sets a new password for the given login and app."
  [login newpassword & [app]]
  (store/with-conn conn
    (account/secret-set-password conn login newpassword app)))

(defn add-setpassword-routes
  "Add a POST routes to the rest handler to set a new password given
  the current credentials either via json or form body."
  []
  (rest/add-routes
   :setpassform (POST "/api/setpass/form" request
                  ((rest/make-setpassword-form-handler user-set-password) request))
   :setpassjson (POST "/api/setpass/json" request
                  ((rest/make-setpassword-json-handler user-set-password) request))
   :setpassforce (POST "/api/setpass-force" request
                   ((rest/make-setpassword-force-form-handler user-force-set-password) request))))

(defn add-listapps-route
  "Add a GET route to retrieve the applications enabled for the
  current user."
  []
  (rest/add-routes
   :listapps (GET "/api/listapps" request
               ((rest/make-list-apps-json-handler) request))))

(defn add-logout-route
  "Add a route that responses with a header to make the clients remove
  the cookie."
  []
  (rest/add-routes
   :logout (ANY "/api/logout" request
             ((rest/make-logout-handler) request))))

(defn add-account-exists-route
  "Add a route that responses to get requests whether an account
  exists or not. The request is expected to have at least a login
  parameter. Optionally an app parameter can be specified."
  []
  (let [exists?
        (fn [login & [app]]
          (store/with-conn conn
            (cond
              (not (account/account-exists? conn login)) false
              (empty? app) true
              (account/app-enabled? conn login app) true
              :else false)))]
    (rest/add-routes
     :account-exists-form
     (GET "/api/account-exists" request
       ((rest/make-account-exists-handler exists?) request)))))

(defn -main [& args]
  (println "Shelter is starting up…")
  (doseq [file args]
    (if (.exists (f/as-file file))
      (do (println (format "Loading file %s…" file))
          (load-file file))
      (println (format "WARNING: Cannot load file '%s', because it does not exist." file))))
  (reset! nrepl-server
          (do
            (println (format "Starting nrepl server on port %d" (config/get :nrepl-port)))
            (nrepl/start-server :port (config/get :nrepl-port))))
  (println "Welcome to shelter."))
