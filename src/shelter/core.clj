;;; core.clj -- shelter main entry point

;; This module contains functions intended for user interaction from
;; within a repl.

(ns shelter.core
  (:require
   [shelter.config :as config]
   [shelter.account :as account]
   [shelter.secret :as secret]
   [shelter.rest :as rest]
   [compojure.core :refer [GET POST]]
   [ring.middleware.json :refer [wrap-json-response wrap-json-params wrap-json-body]]
   [ring.middleware.params :refer [wrap-params]]
   [clojure.java.io :as f]
   [clojure.tools.nrepl.server :as nrepl])
  (:gen-class :main true))

;; default values
(config/set {:nrepl-port 7989})

(defonce ^:private nrepl-server (atom nil))

(defn some-valid?
  "Return true if there is at least one successful secret validation
  in VALIDATE-RESULT. VALIDATE-RESULT is a sequence of
  secret-validation pairs as returned from `account/validate'."
  [validate-result]
  (or (some second validate-result)
      false))

(defn verify
  "Return true if name and password can be verified against the user
  database.

  If APP is given and the account has a separate password for APP it
  is checked against this password. Otherwise the default password of
  the account is used."
  [name password & [app]]
  (some-valid?
   (account/validate name
                     password
                     (and (not-empty app)
                          (account/secret-app-exists? name app) app))))

(defn add-rest-verify-route
  "Add a GET route to the rest handler that verifies account
  credentials given in the url query string. Success or failure is
  encoded in the http status code and additionally in the body using a
  map to be encoded as json."
  []
  (rest/prepend-route
   :verify
   (-> (GET "/verify" [name password app]
            (or (if (not (and name password))
                  {:status 400 :headers {} :body {:success false :message "No credentials given."}})
                (if (verify name password app)
                  {:status  200 :headers {} :body {:success true}}
                  {:status  401 :headers {} :body {:success false}})))
       wrap-params
       wrap-json-response)))

(defn set-password
  "Sets a NEWPW for LOGIN if verification is successful using OLDPW."
  [login oldpw newpw & [appid]]
  (if (verify login oldpw appid)
    (account/update-password login newpw appid)
    {:error "Authentication failed."}))




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
            (nrepl/start-server :port (:nrepl-port @config/config))))
  (println "Welcom to shelter."))
