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
   [compojure.core :refer [GET POST]]
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
          (and app (not (account/app-enabled? conn name app))) false
          :else (let [appsecrets (account/secret-get conn name app)
                      secrets (if (empty? appsecrets)
                                (account/secret-get conn name)
                                appsecrets)
                      result (secret/verify secrets password)]
                  (or (some second result) false)))))

(defn set-password
  "Sets a NEWPW for LOGIN if verification is successful using OLDPW."
  [login oldpw newpw & [appid]]
  (if (verify login oldpw appid)
    (store/with-conn conn
      (account/secret-update-password conn login newpw appid))))


(defn add-verify-routes
  "Add POST routes to the rest handler that verifies account
  credentials given in the url form or json body. Success or failure
  is encoded in the http status code and additionally in the body
  using a map to be encoded as json."
  []
  (rest/add-routes
   :verifyform (POST "/api/verify/form" request
                 ((rest/make-verify-form-handler verify) request))
   :verifyjson (POST "/api/verify/json" request
                 ((rest/make-verify-json-handler verify) request))))

(defn add-setpassword-routes
  "Add a POST routes to the rest handler to set a new password given
  the current credentials either via json or form body."
  []
  (rest/add-routes
   :setpassform (POST "/api/setpass/form" request
                  ((rest/make-setpassword-form-handler set-password) request))
   :setpassjson (POST "/api/setpass/json" request
                  ((rest/make-setpassword-json-handler set-password) request))))


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
